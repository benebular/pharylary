from psychopy import visual, core, event
import random

# Create a window to display the text
win = visual.Window()

# Set the total number of trials
total_trials = 10

# Create a list of trial numbers
trial_order = list(range(1, total_trials + 1))

# Shuffle the order of the trials
random.shuffle(trial_order)

# Create a text stimulus for the instructions
instruction_text = visual.TextStim(win, text="مرحبا بك في الدراسة\n\nهذه التجربة تتكون من " + str(total_trials) + " تجارب.", font="Arial",
                                   pos=(0, 0), height=0.1, wrapWidth=None, color=(-1,-1,-1))

# Create a text stimulus for the counter
counter_text = visual.TextStim(win, text="", font="Arial", pos=(0, -0.5), height=0.08)

# Display the instructions and counter
instruction_text.draw()
counter_text.draw()
win.flip()

# Wait for a response from the participant
event.waitKeys()

# Perform the trials
for trial in trial_order:
    # Update the counter text
    counter_text.text = "التجربة " + str(trial) + " من " + str(total_trials)
    
    # Display the updated counter
    instruction_text.draw()
    counter_text.draw()
    win.flip()
    
    # Wait for a response from the participant
    event.waitKeys()

# Close the window
win.close()