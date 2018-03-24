#!/usr/bin/env python3

import imageio
import numpy as np
import random

# millimeters
WIDTH = 512
HEIGHT = 512

# m/s
MAX_Y_VEL = 5
MAX_X_VEL = 2
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

    initXMM = width / 2
    initYMM = height / 2

    # meters
    initX = initXMM / 1000
    initY = initYMM / 1000

    # m/s
    initXVel = random.random() * MAX_X_VEL * random.choice([-1, 1])
    # Give an upward velocity bias
    initYVel = - random.random() * MAX_Y_VEL

    sequence = []

    t = 0
    while True:
        x, y = coordsForInitialValues(initX, initY, initXVel, initYVel, t)
        xMM = x * 1000
        yMM = y * 1000

        if xMM < 0:
            return sequence, 'left'
        elif xMM >= width:
            return sequence, 'right'
        elif yMM < 0:
            return sequence, 'up'
        elif yMM >= height:
            return sequence, 'down'

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

def generateTestCaseGif(frameCount):
    MIN_FRAMES = 10
    sequence = []
    while len(sequence) < frameCount:
        sequence, direction = generateMovieSequence(WIDTH, HEIGHT)
        print(len(sequence))
    # Only keep the first MIN_FRAMES frames
    sequence = sequence[:frameCount]

    generateGifFromSequence('./test.gif', sequence)
    return direction

print(generateTestCaseGif(10))
