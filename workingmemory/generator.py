#!/usr/bin/env python3

import imageio
import numpy as np
import random

# millimeters
WIDTH = 512
HEIGHT = 512

# m/s
MAX_VEL = 2
# m/s/s
G       = 9.8
# frames/s
FPS     = 100

def coordsForInitialValues(initX, initY, initXVel, initYVel, t):
    x = initX + initXVel * t
    y = initY + initYVel * t + (1/2) * G * (t**2)
    return x, y

# Each pixel represents one 1 millimeter
def generateMovieSequence(width, height):
    # millimeters
    circumferance = 2 * width + 2 * height
    initEdge = random.random() * circumferance

    if initEdge < width:
        initXMM = initEdge
        initYMM = 0
    elif initEdge < width + height:
        initXMM = width
        initYMM = initEdge - width
    elif initEdge < 2 * width + height:
        initXMM = initEdge - height - width
        initYMM = height
    else:
        initXMM = 0
        initYMM = initEdge - height - width * 2

    # meters
    initX = initXMM / 1000
    initY = initYMM / 1000

    # m/s
    initXVel = random.random() * MAX_VEL * random.choice([-1, 1])
    # Give an upward velocity bias
    initYVel = random.random() * (MAX_VEL - 0.5) * random.choice([-1, 1]) - 0.5

    sequence = []

    t = 0
    while True:
        x, y = coordsForInitialValues(initX, initY, initXVel, initYVel, t)
        xMM = x * 1000
        yMM = y * 1000

        if xMM < 0 or xMM >= width or yMM < 0 or yMM >= height:
            return sequence

        sequence.append((int(xMM), int(yMM)))

        t += 1 / float(FPS)

def generateGifFromSequence(impath, sequence):
    frames = []

    for point in sequence:
        frame = np.zeros((WIDTH, HEIGHT), dtype=np.int)
        x, y = point
        frame[y, x] = 255
        frames.append(frame)

    imageio.mimwrite(impath, frames, format='GIF', duration=0.1, loop=0)

MIN_FRAMES = 10
sequence = []
while len(sequence) < MIN_FRAMES:
    sequence = generateMovieSequence(WIDTH, HEIGHT)
# Only keep the first MIN_FRAMES frames
sequence = sequence[:MIN_FRAMES]

generateGifFromSequence('./test.gif', sequence)

