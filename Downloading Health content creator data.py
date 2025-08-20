

from googleapiclient.discovery import build
from datetime import datetime
import csv
import re

api_key = "API_KEY" #obtained from Google Cloud
youtube = build('youtube','v3',developerKey = api_key)
request = youtube.channels().list(
    part = 'statistics',
forHandle= "@creator_handle") #@creatorhandle found on the YouTube channel of the creator

response = request.execute()

print(response) ###provides information on channel ID

def parse_duration_iso8601(duration_str): ##converts video duration to seconds##
    """Parses ISO 8601 duration like PT1H2M3S into seconds."""
    pattern = re.compile(r'PT(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)S)?')
    match = pattern.match(duration_str)
    if not match:
        return 0
    hours, minutes, seconds = match.groups(default="0")
    return int(hours) * 3600 + int(minutes) * 60 + int(seconds)

def get_channel_videos(api_key, channel_id, start_date, end_date):
    youtube = build('youtube', 'v3', developerKey=api_key)
    
    channel_response = youtube.channels().list(
        part='contentDetails',
        id=channel_id
    ).execute()

    uploads_playlist_id = channel_response['items'][0]['contentDetails']['relatedPlaylists']['uploads']
    videos = []
    next_page_token = None

    while True:
        playlist_response = youtube.playlistItems().list(
            part='contentDetails',
            playlistId=uploads_playlist_id,
            maxResults=50,
            pageToken=next_page_token
        ).execute()

        for item in playlist_response['items']:
            video_id = item['contentDetails']['videoId']
            video_date = item['contentDetails']['videoPublishedAt']
            pub_date = datetime.fromisoformat(video_date.replace('Z', '')).replace(tzinfo=None)

            if start_date <= pub_date <= end_date:
                videos.append(video_id)

        next_page_token = playlist_response.get('nextPageToken')
        if not next_page_token:
            break

    return videos

def get_video_details(api_key, video_ids):
    youtube = build('youtube', 'v3', developerKey=api_key)
    all_details = []

    for i in range(0, len(video_ids), 50):
        ids_chunk = video_ids[i:i + 50]
        video_response = youtube.videos().list(
            part='snippet,statistics,contentDetails',
            id=','.join(ids_chunk)
        ).execute()

        for item in video_response['items']:
            snippet = item['snippet']
            stats = item.get('statistics', {})
            content = item['contentDetails']
            duration_seconds = parse_duration_iso8601(content['duration'])

            video_data = {
                'videoId': item['id'],
                'title': snippet['title'],
                'publishedAt': snippet['publishedAt'],
                'views': int(stats.get('viewCount', 0)),
                'likes': int(stats.get('likeCount', 0)),
                'comments': int(stats.get('commentCount', 0)),
                'duration_seconds': duration_seconds
            }
            all_details.append(video_data)

    return all_details

def export_to_csv(data, filename='DrNancy.csv'):
    if not data:
        print("No data to export.")
        return

    with open(filename, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=data[0].keys())
        writer.writeheader()
        writer.writerows(data)
    print(f"✅ Data exported to {filename}")


# Example usage
API_KEY = 'API_key' #API key on Google Cloud
CHANNEL_ID = 'Channel_ID'  # Replace with your actual channel ID

start = datetime(2024, 3, 25) #6 months before "Pulse" date
end = datetime(2025, 3, 26) #6 months after "Pulse" date

video_ids = get_channel_videos(API_KEY, CHANNEL_ID, start, end)
details = get_video_details(API_KEY, video_ids)
export_to_csv(details)



