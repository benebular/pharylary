## whisper transcription and translation for arabic passive listen
# blang@ucsd.edu

import torch
import whisper

# Check if CUDA is available for GPU acceleration
device = 'cuda' if torch.cuda.is_available() else 'cpu'

# Load the Whisper model
model = whisper.load_model('large').to(device)  # Change model size if needed

# Path to the audio file
audio_path = "I:\My Drive\Dissertation\chapter3\podcast\9A7F4D7A-CBDC-4583-8B99-DB41A01EC155.wav"  # Replace with your actual file path

# Transcribe the audio and detect language
result = model.transcribe(audio_path)

# Get the detected language
detected_language = result["language"]
transcribed_text = result["text"]

# Translate the transcription into English
translated_text = model.transcribe(audio_path, task="translate")["text"]

# Save original transcription
original_file = "transcription_original.txt"
with open(original_file, "w", encoding="utf-8") as f:
    f.write(f"Detected Language: {detected_language}\n\n")
    f.write(transcribed_text)

# Save translated transcription
translated_file = "transcription_translated.txt"
with open(translated_file, "w", encoding="utf-8") as f:
    f.write(translated_text)

print(f"Transcription saved to {original_file}")
print(f"English translation saved to {translated_file}")
