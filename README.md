# Image Processing Functions in F#
Author: Rimsha Rizvi

## Description
This repository contains F# functions for processing images in PPM format. The user can select an operation from the menu to manipulate the image. Operations include grayscaling, thresholding, flipping, rotating, and edge detection.

## Operations
1. **Grayscale**: Converts the image to grayscale using a weighted average calculation to account for the human eye's perception of brightness in red, green, and blue colors.
2. **Threshold**: Increases image separation by setting RGB values above a given threshold to the color depth and values below the threshold to 0.
3. **FlipHorizontal**: Flips the image horizontally, making what's on the left appear on the right and vice versa.
4. **RotateRight90**: Rotates the image 90 degrees to the right.
5. **Edge Detection**: Replaces each pixel in the image with a black pixel if it contains an "edge" and a white pixel otherwise. An edge occurs when the color difference between a pixel and its neighboring pixels is greater than a given threshold.

If you would like to use this work for educational or other non-commercial purposes, don't hesitate to get in touch with the author for permission.
