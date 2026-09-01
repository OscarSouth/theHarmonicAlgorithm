import sys
import requests
import json

def send_to_listener(message):
    try:
        # URL of the listener (Flask app)
        url = 'http://localhost:5000/tidalcycles'
        
        # Prepare the data to send
        data = {
            'output': message
        }
        
        # Send POST request to the Flask listener
        response = requests.post(url, json=data)
        
        # Check response
        if response.status_code == 200:
            print(f"Successfully sent: '{message}'")
        else:
            print(f"Failed to send. Status code: {response.status_code}")
    
    except Exception as e:
        print(f"Error: {e}")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python tester.py 'Your message here'")
    else:
        message = sys.argv[1]
        send_to_listener(message)
