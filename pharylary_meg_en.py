from psychopy import prefs
prefs.general['audioLib'] = ['PTB']
prefs.hardware['audioLatencyMode'] = 3

import psychtoolbox as ptb

from psychopy import visual, core, event, sound
from psychopy import parallel  # For sending TTL pulses
import time  # For logging timestamps
import pandas as pd  # For reading the spreadsheet
import os
import random

## Set up parallel port for TTL pulses
#port = parallel.ParallelPort(address=0x0378)  # Update address as needed
# 0x3FF8

def stim_trigger(sound_obj, n=8):
    # Set up parallel port for TTL pulses
    p_port = parallel.ParallelPort(address='0x0378')
    p_port.setData(0)
    
    nextFlip = win.getFutureFlipTime(clock='ptb')
    sound_obj.play(when=nextFlip)  # Play sound on the next flip
    
    trigger_value = (1 << n) - 1  # Cumulative bits for n pins (n ∈ [1,8])
    win.callOnFlip(p_port.setData, trigger_value)  # Send trigger on the next flip
    win.flip()
    core.wait(0.01)  # Send 10 ms pulse
    p_port.setData(0)

# Create a window
win = visual.Window([800, 600], color="black", fullscr=False)

# Instructions screen
def show_instructions():
    instructions = visual.TextStim(win, text="Welcome to the experiment!\n\nYou will hear a sound and then choose between two words.\n\nPress any key to begin.", color="white")
    instructions.draw()
    win.flip()
    event.waitKeys()  # Wait for any key press

# Present audio and fixation cross
def present_audio_and_fixation(audio_file, trial_number, log_file, left_word, right_word):
    fixation = visual.TextStim(win, text="+", color="white")
    audio = sound.Sound(audio_file)

    # Log the trial and timestamp
    audio_start_time = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
    with open(log_file, "a") as f:
        f.write(f"Trial {trial_number}, Audio: {audio_file}, LeftWord: {left_word}, RightWord: {right_word}, Audio Start Time: {audio_start_time}\n")
    
    # Use stim_trigger function to send TTL pulse and play audio
    stim_trigger(audio)

    # Wait for audio to finish
    core.wait(audio.getDuration())

# Present two words and collect response
def present_choices_and_get_response(left_word, right_word, trial_number, log_file):
    # Randomly assign left and right positions
    choices = [(left_word, (-0.5, 0)), (right_word, (0.5, 0))]
    random.shuffle(choices)
    
    left_stim = visual.TextStim(win, text=choices[0][0], pos=choices[0][1], color="white")
    right_stim = visual.TextStim(win, text=choices[1][0], pos=choices[1][1], color="white")
    fixation = visual.TextStim(win, text="+", color="white")

    # Draw the words and fixation cross
    fixation.draw()
    left_stim.draw()
    right_stim.draw()
    win.flip()

    # Wait for participant response
    response_start_time = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
    keys = event.waitKeys(keyList=["left", "right", "escape"])

    # Handle responses
    if "escape" in keys:
        core.quit()  # Exit the experiment
    elif "left" in keys:
        response = choices[0][0] if choices[0][1] == (-0.5, 0) else choices[1][0]
    elif "right" in keys:
        response = choices[0][0] if choices[0][1] == (0.5, 0) else choices[1][0]

    # Log the response and timestamp
    response_time = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
    with open(log_file, "a") as f:
        f.write(f"Trial {trial_number}, Response: {response}, Response Start Time: {response_start_time}, Response End Time: {response_time}\n")

    return response

# Main experiment logic
def run_experiment(spreadsheet):
    log_file = "experiment_log.txt"
    with open(log_file, "w") as f:
        f.write("Trial Log\n")

    # Read the spreadsheet
    trials_df = pd.read_csv(spreadsheet)

    show_instructions()

    # Loop through each row in the spreadsheet
    for trial_number, row in trials_df.iterrows():
        audio_file = os.path.join("/Volumes/circe/alldata/dissertation/2/stimuli", row["TargetWordAudioFile"])
        left_word = row["TargetDisplay"]
        right_word = row["DistractorDisplay"]

        present_audio_and_fixation(audio_file, trial_number + 1, log_file, left_word, right_word)
        response = present_choices_and_get_response(left_word, right_word, trial_number + 1, log_file)

# Run the experiment
if __name__ == "__main__":
    spreadsheet_path = "/Volumes/circe/alldata/dissertation/2/garellek_2015_stimuli.csv"  # Update with the correct path to your spreadsheet
    run_experiment(spreadsheet_path)
    win.close()
    core.quit()
