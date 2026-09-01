"""Mirror the Pulsar TidalCycles console into a terminal.

The editor's console view is small and scrolls; this prints the same stream
into a full terminal window alongside the session, which is where generation
traces (`gen'` / `gen''` scoreboards) are actually readable.

The sending half is a local patch to the tidalcycles Pulsar package -- see
scripts/pulsar-apply-patches, which must be re-run after every plugin upgrade
because `ppm install` replaces the package directory wholesale.

Bound to 127.0.0.1: the patch posts to http://localhost:5000, so there is no
reason to expose the console on any other interface. This machine is taken to
venues.

Run:  python3 scripts/tidal-monitor/listener.py
      (live/local/livecode-perform starts it automatically)
"""
from flask import Flask, request
import logging

app = Flask(__name__)

# Suppress werkzeug's per-request INFO lines; only the Tidal output should show.
log = logging.getLogger('werkzeug')
log.setLevel(logging.ERROR)


@app.route('/tidalcycles', methods=['POST'])
def tidalcycles_output():
    data = request.json
    if data and 'output' in data:
        print(data['output'])
    return '', 200


if __name__ == '__main__':
    app.run(host='127.0.0.1', port=5000)
