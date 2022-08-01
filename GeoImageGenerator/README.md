Side project for generating images from a list of geocodes. It processes roughly 1G of geocode data (~40 million lines) in 15 seconds

Compile with:

```shell
gcc -O3 -lm main.c image.c color.c -o geo
```

The output format is PPM for simplicity. Convert to a PNG with your favourite image editor

TODO: gotta go fast https://www.gnu.org/software/parallel/
