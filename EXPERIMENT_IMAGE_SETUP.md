# Experiment Image Setup Guide

## Overview
The experiment has been updated to display image pairs alongside word choices. One image will always correspond to the target word (the one the participant hears in the audio).

## Folder Structure

Create the following folder structure in your repository:
```
/data/images/
  ├── bat.jpg
  ├── pat.jpg
  ├── cap.jpg
  ├── cab.jpg
  ├── heed.jpg
  ├── hid.jpg
  └── [all other image files...]
```

The images will be served from `/data/images/` directory.

## Trial Configuration

Each trial in the `TRIALS` array now requires the following fields:

```javascript
{
  trial_id: 1,
  audio_file: "example_001.wav",
  left_word: "bat",
  right_word: "pat",
  left_image: "bat.jpg",      // Image filename for left option
  right_image: "pat.jpg",     // Image filename for right option
  target_side: "left"         // Which side is the target ("left" or "right")
}
```

### Field Descriptions:
- **trial_id**: Unique identifier for the trial
- **audio_file**: Audio file to play (from `/data/audio/`)
- **left_word**: Word displayed on left button
- **right_word**: Word displayed on right button
- **left_image**: Filename of image for left option (in `/data/images/`)
- **right_image**: Filename of image for right option (in `/data/images/`)
- **target_side**: Which option is the target. Should be `"left"` or `"right"` to indicate which word matches the audio

## Image Display

- Images are displayed as a pair above the word choice buttons
- Each image is accompanied by a label showing the word it represents
- Maximum image size: 200px × 200px
- Images are centered and separated by 20px gap
- Images have rounded corners and a subtle border

## Data Logged

When a participant makes a choice, the following image-related data is sent to Google Sheets:

- **left_image**: The left image filename
- **right_image**: The right image filename
- **choice_image**: The image filename corresponding to the participant's choice
- **target_side**: Which side was the target
- **is_target**: "yes" or "no" - whether the participant chose the target image

## Example Trial Data

```javascript
const TRIALS = [
  { 
    trial_id: 1, 
    audio_file: "arabic_word_001.wav", 
    left_word: "خ", 
    right_word: "ح",
    left_image: "pharyngeal.jpg", 
    right_image: "uvular.jpg",
    target_side: "left"
  },
  { 
    trial_id: 2, 
    audio_file: "arabic_word_002.wav", 
    left_word: "ح", 
    right_word: "خ",
    left_image: "uvular.jpg", 
    right_image: "pharyngeal.jpg",
    target_side: "right"
  },
];
```

## CSS Styling

The images use these CSS classes:
- `.image-pair`: Container for both images (flexbox layout)
- `.image-container`: Container for each individual image with label
- `.image-label`: Text label below each image

You can modify the styles in the `<style>` section of `experiment.html` to adjust:
- Image size (max-width, max-height)
- Spacing between images (gap)
- Border and rounding style
- Label font size and color

## Notes

- If an image filename is not provided or missing, that side will display without an image (just the word)
- Images are lazy-loaded as needed when trials load
- Broken image links will show a broken image icon in the browser
- Make sure image filenames match exactly (case-sensitive on web servers)
