import csv
import arabic_reshaper
from arabic_reshaper import ArabicReshaper
configuration = {
    'delete_harakat': False,
    'shift_harakat_position=True'
    'support_ligatures': True,
    'RIAL SIGN': True,  # Replace ر ي ا ل with ﷼
}
reshaper = ArabicReshaper(configuration=configuration)

from bidi.algorithm import get_display

from PIL import Image, ImageDraw, ImageFont

import os
experiment_dir = '/Users/bcl/Library/CloudStorage/GoogleDrive-blang@ucsd.edu/My Drive/Dissertation/experiment1/img_stims/' #makes a variable that is the experiment directory
os.chdir(experiment_dir) #tells the os to go to the directory every time

width = 750
height = 200
font = ImageFont.truetype('/Users/bcl/Library/Fonts/Amiri-Bold.ttf', size=60)


with open('PharyLary Stimuli - Export', 'r') as f:
    reader = csv.reader(f)
    i = 0
    for [message] in reader:
        i += 1

        # text_to_be_reshaped = 'اللغة العربية رائعة'
        reshaped_text = reshaper.reshape(message)
        bidi_text = get_display(reshaped_text)
        img = Image.new('RGB', (width, height), color='white')

        imgDraw = ImageDraw.Draw(img)

        # textWidth, textHeight = imgDraw.textsize(bidi_text)
        # xText = (width - textWidth) / 2
        # yText = (height - textHeight) / 2

        imgDraw.text((50, 50), bidi_text, font=font, fill=(0, 0, 0), align='center')

        img.save(f'result{i}.jpg')