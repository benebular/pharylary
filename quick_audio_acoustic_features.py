#!/usr/bin/env python3
"""Analyze WAV files with Praat and plot F0, intensity, and CPP distributions."""

from __future__ import annotations

import argparse
import concurrent.futures
import math
import os
import statistics
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import parselmouth
from parselmouth.praat import call


DEFAULT_AUDIO_DIR = Path(__file__).resolve().parent / "data" / "audio"
DEFAULT_OUTPUT = Path(__file__).resolve().parent / "audio_acoustic_feature_summary.png"


def classify_file(path: Path) -> str | None:
    name = path.stem.lower()
    if name.endswith("_non-creaky"):
        return "non-creaky"
    if name.endswith("_creaky"):
        return "creaky"
    return None


def mean(values: list[float]) -> float:
    return statistics.mean(values)


def kde(values: list[float], grid_points: int = 250):
    if len(values) == 1:
        center = values[0]
        spread = max(abs(center) * 0.05, 0.01)
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


def extract_features(wav_path: Path) -> dict[str, float]:
    sound = parselmouth.Sound(str(wav_path))

    pitch = sound.to_pitch(pitch_floor=60, pitch_ceiling=500)
    f0_values = pitch.selected_array["frequency"]
    f0_values = [value for value in f0_values if value > 0]
    if not f0_values:
        raise ValueError(f"No voiced pitch frames found in {wav_path.name}")
    f0_mean = mean(f0_values)

    # Praat intensity in dB for the full file; use the direct file-level query.
    intensity_db = sound.get_intensity()

    power_cepstrogram = call(sound, "To PowerCepstrogram...", 60, 0.002, 5000, 50)
    cpps = call(
        power_cepstrogram,
        "Get CPPS...",
        False,
        0.01,
        0.01,
        75,
        300,
        0.01,
        "parabolic",
        0.001,
        0.05,
        "straight",
        "robust slow",
    )

    return {
        "f0_mean": float(f0_mean),
        "intensity_db": float(intensity_db),
        "cpps": float(cpps),
    }


def extract_features_for_path(wav_path: Path) -> tuple[str, dict[str, float] | None, str | None]:
    try:
        return wav_path.name, extract_features(wav_path), None
    except Exception as exc:
        return wav_path.name, None, str(exc)


def plot_density(ax, values_by_label: dict[str, list[float]], title: str, xlabel: str, colors: dict[str, str]):
    all_values = [value for values in values_by_label.values() for value in values]
    ax.hist(
        all_values,
        bins=24,
        density=True,
        color="0.75",
        alpha=0.25,
        edgecolor="white",
        label="All files",
    )

    for label in ("non-creaky", "creaky"):
        xs, ys = kde(values_by_label[label])
        ax.fill_between(xs, ys, color=colors[label], alpha=0.22)
        ax.plot(xs, ys, color=colors[label], linewidth=2, label=label)

    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel("Density")
    ax.legend(frameon=False)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Analyze WAV files with Praat and plot F0, intensity, and CPP density plots."
    )
    parser.add_argument("audio_dir", nargs="?", default=str(DEFAULT_AUDIO_DIR))
    parser.add_argument("--output", default=str(DEFAULT_OUTPUT))
    parser.add_argument("--show", action="store_true", help="Open the figure after saving.")
    args = parser.parse_args()

    audio_dir = Path(args.audio_dir).expanduser().resolve()
    output_path = Path(args.output).expanduser().resolve()

    if not audio_dir.exists():
        raise SystemExit(f"Audio directory not found: {audio_dir}")

    records: list[dict[str, float | str]] = []
    skipped: list[str] = []
    wav_paths = [wav_path for wav_path in sorted(audio_dir.glob("*.wav")) if classify_file(wav_path) is not None]
    skipped.extend(wav_path.name for wav_path in sorted(audio_dir.glob("*.wav")) if classify_file(wav_path) is None)

    worker_count = min(os.cpu_count() or 1, len(wav_paths))
    with concurrent.futures.ProcessPoolExecutor(max_workers=worker_count) as executor:
        future_map = {
            executor.submit(extract_features_for_path, wav_path): wav_path
            for wav_path in wav_paths
        }
        for future in concurrent.futures.as_completed(future_map):
            wav_name, features, error = future.result()
            if error is not None or features is None:
                skipped.append(f"{wav_name} ({error})")
                continue

            label = classify_file(future_map[future])
            if label is None:
                skipped.append(wav_name)
                continue

            records.append({"file": wav_name, "label": label, **features})

    records.sort(key=lambda item: item["file"])

    if not records:
        raise SystemExit(f"No usable WAV files found in {audio_dir}")

    grouped = {
        "non-creaky": {"f0_mean": [], "intensity_db": [], "cpps": []},
        "creaky": {"f0_mean": [], "intensity_db": [], "cpps": []},
    }
    for record in records:
        label = record["label"]
        grouped[label]["f0_mean"].append(float(record["f0_mean"]))
        grouped[label]["intensity_db"].append(float(record["intensity_db"]))
        grouped[label]["cpps"].append(float(record["cpps"]))

    print(f"Scanned {len(records)} WAV files in {audio_dir}")
    for label in ("creaky", "non-creaky"):
        print(
            f"{label:11s} n={len(grouped[label]['f0_mean']):3d} "
            f"f0_mean={mean(grouped[label]['f0_mean']):.2f} Hz "
            f"intensity={mean(grouped[label]['intensity_db']):.2f} dB "
            f"cpps={mean(grouped[label]['cpps']):.2f} dB"
        )

    if skipped:
        print(f"Skipped {len(skipped)} files")

    colors = {"non-creaky": "#1f77b4", "creaky": "#d95f02"}
    fig, axes = plt.subplots(1, 3, figsize=(16, 5), constrained_layout=True)
    fig.suptitle("Praat acoustic feature summary", fontsize=14, fontweight="bold")

    plot_density(axes[0], {label: grouped[label]["f0_mean"] for label in grouped}, "Mean F0", "Hz", colors)
    plot_density(axes[1], {label: grouped[label]["intensity_db"] for label in grouped}, "Mean dB energy", "dB", colors)
    plot_density(axes[2], {label: grouped[label]["cpps"] for label in grouped}, "Mean CPPS", "dB", colors)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_path, dpi=200, bbox_inches="tight")
    print(f"Saved plot to {output_path}")

    if args.show:
        plt.show()


if __name__ == "__main__":
    main()