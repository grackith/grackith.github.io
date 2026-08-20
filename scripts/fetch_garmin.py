#!/usr/bin/env python3
"""Pull the last 7 days from Garmin Connect into data/health.json.

Run by .github/workflows/garmin-sync.yml on a daily cron. Credentials come
from the GARMIN_EMAIL / GARMIN_PASSWORD repository secrets -- never hardcode
them here.

Every metric is fetched defensively: Garmin's endpoints change without notice
and a single 404 should degrade one number, not lose the whole file.
"""

import datetime
import json
import os
import pathlib
import sys

from garminconnect import Garmin

OUT = pathlib.Path(__file__).resolve().parent.parent / "data" / "health.json"
DAYS = 7


def safe(fn, default=None):
    """Call a Garmin endpoint, swallowing anything it throws."""
    try:
        return fn()
    except Exception as exc:  # noqa: BLE001 - any failure means "no data"
        print(f"  ! {fn.__name__ if hasattr(fn, '__name__') else fn}: {exc}", file=sys.stderr)
        return default


def day_record(api, day):
    ds = day.isoformat()
    stats = safe(lambda: api.get_stats(ds), {}) or {}

    body_battery = None
    bb = safe(lambda: api.get_body_battery(ds, ds), []) or []
    if bb:
        values = bb[0].get("bodyBatteryValuesArray") or []
        if values:
            # each entry is [timestamp, status, level, version]; take the last level
            levels = [v[2] for v in values if len(v) > 2 and v[2] is not None]
            if levels:
                body_battery = levels[-1]

    sleep_hours = None
    sleep = safe(lambda: api.get_sleep_data(ds), {}) or {}
    seconds = (sleep.get("dailySleepDTO") or {}).get("sleepTimeSeconds")
    if seconds:
        sleep_hours = round(seconds / 3600, 1)

    return {
        "date": ds,
        "steps": stats.get("totalSteps"),
        "restingHeartRate": stats.get("restingHeartRate"),
        "bodyBattery": body_battery,
        "sleepHours": sleep_hours,
        "stress": stats.get("averageStressLevel"),
        "activeCalories": stats.get("activeKilocalories"),
        "intensityMinutes": (stats.get("moderateIntensityMinutes") or 0)
        + (stats.get("vigorousIntensityMinutes") or 0),
        "stepGoal": stats.get("dailyStepGoal"),
    }


def main():
    email = os.environ.get("GARMIN_EMAIL")
    password = os.environ.get("GARMIN_PASSWORD")
    if not email or not password:
        sys.exit("GARMIN_EMAIL and GARMIN_PASSWORD must be set")

    api = Garmin(email, password)
    api.login()

    today = datetime.date.today()
    days = [today - datetime.timedelta(days=i) for i in range(DAYS - 1, -1, -1)]

    week = []
    for day in days:
        print(f"fetching {day}")
        week.append(day_record(api, day))

    latest = week[-1]
    # a same-day pull can land before the watch has synced; fall back a day
    if not latest.get("steps") and len(week) > 1:
        latest = week[-2]

    doc = {
        "updated": datetime.datetime.now(datetime.timezone.utc).isoformat(
            timespec="seconds"
        ),
        "source": "garmin",
        "stepGoal": latest.get("stepGoal") or 10000,
        "today": {
            "steps": latest.get("steps"),
            "restingHeartRate": latest.get("restingHeartRate"),
            "bodyBattery": latest.get("bodyBattery"),
            "sleepHours": latest.get("sleepHours"),
            "stress": latest.get("stress"),
            "activeCalories": latest.get("activeCalories"),
            "intensityMinutes": latest.get("intensityMinutes"),
        },
        "week": [
            {
                k: v
                for k, v in d.items()
                if k in ("date", "steps", "restingHeartRate", "bodyBattery", "sleepHours")
            }
            for d in week
        ],
    }

    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(doc, indent=2) + "\n")
    print(f"wrote {OUT}")


if __name__ == "__main__":
    main()
