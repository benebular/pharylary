#!/usr/bin/env python3
"""Summarize WAV lengths in data/audio and plot creaky vs non-creaky groups."""

from __future__ import annotations

import argparse
import math
import statistics
import wave
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt


DEFAULT_AUDIO_DIR = Path(__file__).resolve().parent / "data" / "audio"
DEFAULT_OUTPUT = Path(__file__).resolve().parent / "audio_length_summary.png"


def classify_file(path: Path) -> str | None:
    name = path.stem.lower()
    if name.endswith("_non-creaky"):
        return "non-creaky"
    if name.endswith("_creaky"):
        return "creaky"
    return None


def wav_duration_seconds(path: Path) -> float:
    with wave.open(str(path), "rb") as wav_file:
        frames = wav_file.getnframes()
        frame_rate = wav_file.getframerate()
        if frame_rate <= 0:
            raise ValueError(f"Invalid frame rate in {path}")
        return frames / frame_rate


def kde(values, grid_points: int = 250):
    if len(values) == 1:
        center = values[0]
        spread = max(center * 0.05, 0.01)
        xs = [center + spread * (i - grid_points // 2) / (grid_points // 4) for i in range(grid_points)]
        ys = [math.exp(-0.5 * ((x - center) / spread) ** 2) / (spread * math.sqrt(2 * math.pi)) for x in xs]
        return xs, ys

    stdev = statistics.stdev(values)
    bandwidth = 1.06 * stdev * (len(values) ** (-1 / 5)) if stdev > 0 else 0.1
    bandwidth = max(bandwidth, 0.01)

    xmin = min(values)
    xmax = max(values)
    padding = max((xmax - xmin) * 0.15, bandwidth * 3)
    start = xmin - padding
    stop = xmax + padding
    step = (stop - start) / (grid_points - 1)
    xs = [start + i * step for i in range(grid_points)]

    coef = 1.0 / (len(values) * bandwidth * math.sqrt(2 * math.pi))
    ys = []
    for x in xs:
        total = 0.0
        for value in values:
            z = (x - value) / bandwidth
            total += math.exp(-0.5 * z * z)
        ys.append(coef * total)

    return xs, ys


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Check WAV lengths in data/audio and plot creaky vs non-creaky groups."
    )
    parser.add_argument("audio_dir", nargs="?", default=str(DEFAULT_AUDIO_DIR))
    parser.add_argument("--output", default=str(DEFAULT_OUTPUT))
    parser.add_argument("--show", action="store_true", help="Open the plot window after saving.")
    args = parser.parse_args()

    audio_dir = Path(args.audio_dir).expanduser().resolve()
    output_path = Path(args.output).expanduser().resolve()

    if not audio_dir.exists():
        raise SystemExit(f"Audio directory not found: {audio_dir}")

    records = []
    skipped = []
    for wav_path in sorted(audio_dir.glob("*.wav")):
        label = classify_file(wav_path)
        if label is None:
            skipped.append(wav_path.name)
            continue
        duration = wav_duration_seconds(wav_path)
        records.append({"name": wav_path.name, "label": label, "duration": duration})

    if not records:
        raise SystemExit(f"No labeled WAV files found in {audio_dir}")

    groups = {"creaky": [], "non-creaky": []}
    for record in records:
        groups[record["label"]].append(record["duration"])

    print(f"Scanned {len(records)} WAV files in {audio_dir}")
    for label in ("creaky", "non-creaky"):
        durations = groups[label]
        if durations:
            print(
                f"{label:11s} n={len(durations):3d} mean={statistics.mean(durations):.3f}s "
                f"min={min(durations):.3f}s max={max(durations):.3f}s"
            )
        else:
            print(f"{label:11s} n=0")

    if skipped:
        print(f"Skipped {len(skipped)} unlabeled files")

    labels = ["non-creaky", "creaky"]
    colors = {"non-creaky": "#1f77b4", "creaky": "#d95f02"}

    fig, (ax_mean, ax_density) = plt.subplots(1, 2, figsize=(12, 5), constrained_layout=True)
    fig.suptitle("WAV length summary", fontsize=14, fontweight="bold")

    means = [statistics.mean(groups[label]) for label in labels]
    ax_mean.bar(labels, means, color=[colors[label] for label in labels], width=0.6)
    ax_mean.set_title("Average length")
    ax_mean.set_ylabel("Seconds")
    ax_mean.set_ylim(0, max(means) * 1.25)
    for idx, value in enumerate(means):
        ax_mean.text(idx, value, f"{value:.3f}", ha="center", va="bottom", fontsize=9)

    all_durations = [record["duration"] for record in records]
    ax_density.hist(
        all_durations,
        bins=25,
        density=True,
        color="0.7",
        alpha=0.25,
        edgecolor="white",
        label="All files",
    )

    for label in labels:
        xs, ys = kde(groups[label])
        ax_density.fill_between(xs, ys, color=colors[label], alpha=0.25)
        ax_density.plot(xs, ys, color=colors[label], linewidth=2, label=f"{label} KDE")

    ax_density.set_title("Length distribution")
    ax_density.set_xlabel("Seconds")
    ax_density.set_ylabel("Density")
    ax_density.legend(frameon=False)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_path, dpi=200, bbox_inches="tight")
    print(f"Saved plot to {output_path}")

    if args.show:
        plt.show()


if __name__ == "__main__":
    main()