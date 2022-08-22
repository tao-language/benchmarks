import numpy as np


def mandelbrot(width: int, height: int, iterations: int) -> np.ndarray:
    x = np.linspace(-2, 1, width)
    y = np.linspace(-1, 1, height)
    c = [x] + 1j * y[:, None]
    z = np.zeros(c.shape, dtype=complex)

    values = np.full(z.shape, iterations, dtype=int)
    mask = np.full(z.shape, True, dtype=bool)
    for i in range(iterations):
        z[mask] = z[mask] ** 2 + c[mask]
        new_mask = abs(z) < 2
        values[new_mask ^ mask] = i
        mask = new_mask
    return values


def print_mandelbrot_ascii(width: int, height: int, palette: str):
    indices = mandelbrot(width, height, len(palette) - 1)
    chars = np.take(np.array(list(palette)), indices)
    for row in chars.view(f"U{width}"):
        print(row[0])


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("width", type=int)
    parser.add_argument("height", type=int)
    args = parser.parse_args()

    # http://paulbourke.net/dataformats/asciiart/
    # palette = " .:-=+*#%@"
    palette = " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
    print_mandelbrot_ascii(args.width, args.height, palette)
