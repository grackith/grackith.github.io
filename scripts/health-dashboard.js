/* health-dashboard.js — renders data/health.json into the .FUN page.
   Data is refreshed by .github/workflows/garmin-sync.yml. */
(function () {
  'use strict';

  var SERIES_STEPS = '#2a78d6';
  var SERIES_HR    = '#e34948';
  var DAY = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];

  function el(tag, cls, html) {
    var n = document.createElement(tag);
    if (cls) n.className = cls;
    if (html != null) n.innerHTML = html;
    return n;
  }

  function fmt(n) {
    return n == null ? '--' : n.toLocaleString();
  }

  function dayLabel(iso) {
    var d = new Date(iso + 'T12:00:00');
    return DAY[d.getDay()];
  }

  /* bar with rounded top corners, square foot on the baseline */
  function barPath(x, y, w, h, r) {
    r = Math.min(r, w / 2, h);
    return 'M' + x + ',' + (y + h) +
           'V' + (y + r) +
           'a' + r + ',' + r + ' 0 0 1 ' + r + ',' + -r +
           'h' + (w - 2 * r) +
           'a' + r + ',' + r + ' 0 0 1 ' + r + ',' + r +
           'V' + (y + h) + 'Z';
  }

  function svg(tag, attrs) {
    var n = document.createElementNS('http://www.w3.org/2000/svg', tag);
    for (var k in attrs) n.setAttribute(k, attrs[k]);
    return n;
  }

  /* ---------------- tooltip ---------------- */
  function makeTip(host) {
    var tip = el('div', 'hd-tip');
    tip.setAttribute('role', 'status');
    host.appendChild(tip);
    return {
      show: function (evt, html) {
        tip.innerHTML = html;
        tip.classList.add('on');
        var hb = host.getBoundingClientRect();
        var x = evt.clientX - hb.left;
        var y = evt.clientY - hb.top;
        tip.style.left = Math.max(4, Math.min(x, hb.width - tip.offsetWidth - 4)) + 'px';
        tip.style.top = Math.max(0, y - tip.offsetHeight - 10) + 'px';
      },
      hide: function () { tip.classList.remove('on'); }
    };
  }

  /* ---------------- steps: column chart ---------------- */
  function stepsChart(week, goal) {
    var W = 320, H = 150, padB = 22, padT = 18, padL = 4, padR = 4;
    var vals = week.map(function (d) { return d.steps || 0; });
    var max = Math.max.apply(null, vals.concat([goal || 0])) * 1.08 || 1;
    var plotH = H - padB - padT;
    var slot = (W - padL - padR) / week.length;
    var bw = slot - 2;                      // 2px surface gap between bars

    var s = svg('svg', {
      viewBox: '0 0 ' + W + ' ' + H, class: 'hd-chart',
      role: 'img', 'aria-label': 'Daily steps over the last seven days'
    });

    if (goal) {
      var gy = padT + plotH - (goal / max) * plotH;
      s.appendChild(svg('line', {
        x1: padL, x2: W - padR, y1: gy, y2: gy,
        stroke: '#c9ccd1', 'stroke-width': 1, 'stroke-dasharray': '3 3'
      }));
    }

    var maxIdx = vals.indexOf(Math.max.apply(null, vals));
    week.forEach(function (d, i) {
      var v = d.steps || 0;
      var h = Math.max((v / max) * plotH, v > 0 ? 2 : 0);
      var x = padL + i * slot + 1;
      var y = padT + plotH - h;

      var p = svg('path', { d: barPath(x, y, bw, h, 4), fill: SERIES_STEPS, class: 'hd-bar' });
      p.dataset.label = dayLabel(d.date) + ' &middot; ' + fmt(d.steps) + ' steps';
      s.appendChild(p);

      // selective direct label: the week's best day only
      if (i === maxIdx && v > 0) {
        var t = svg('text', { x: x + bw / 2, y: y - 5, class: 'hd-val', 'text-anchor': 'middle' });
        t.textContent = fmt(v);
        s.appendChild(t);
      }

      var ax = svg('text', { x: x + bw / 2, y: H - 7, class: 'hd-axis', 'text-anchor': 'middle' });
      ax.textContent = dayLabel(d.date);
      s.appendChild(ax);
    });
    return s;
  }

  /* ---------------- resting HR: line chart ---------------- */
  function hrChart(week) {
    var pts = week.filter(function (d) { return d.restingHeartRate != null; });
    if (pts.length < 2) return null;

    var W = 320, H = 130, padB = 22, padT = 20, padL = 6, padR = 6;
    var vals = pts.map(function (d) { return d.restingHeartRate; });
    var lo = Math.min.apply(null, vals) - 2, hi = Math.max.apply(null, vals) + 2;
    var plotH = H - padB - padT;
    var slot = (W - padL - padR) / (pts.length - 1 || 1);
    var y = function (v) { return padT + plotH - ((v - lo) / (hi - lo || 1)) * plotH; };
    var x = function (i) { return padL + i * slot; };

    var s = svg('svg', {
      viewBox: '0 0 ' + W + ' ' + H, class: 'hd-chart',
      role: 'img', 'aria-label': 'Resting heart rate over the last seven days'
    });

    [0, 0.5, 1].forEach(function (f) {
      var gy = padT + plotH * f;
      s.appendChild(svg('line', {
        x1: padL, x2: W - padR, y1: gy, y2: gy, stroke: '#eceef1', 'stroke-width': 1
      }));
    });

    var d = pts.map(function (p, i) {
      return (i ? 'L' : 'M') + x(i) + ',' + y(p.restingHeartRate);
    }).join(' ');
    s.appendChild(svg('path', {
      d: d, fill: 'none', stroke: SERIES_HR, 'stroke-width': 2,
      'stroke-linejoin': 'round', 'stroke-linecap': 'round'
    }));

    pts.forEach(function (p, i) {
      // 2px surface ring keeps overlapping markers separable
      s.appendChild(svg('circle', {
        cx: x(i), cy: y(p.restingHeartRate), r: 5,
        fill: SERIES_HR, stroke: '#fff', 'stroke-width': 2
      }));
      var hit = svg('circle', { cx: x(i), cy: y(p.restingHeartRate), r: 12, fill: 'transparent', class: 'hd-dot' });
      hit.dataset.label = dayLabel(p.date) + ' &middot; ' + p.restingHeartRate + ' bpm resting';
      s.appendChild(hit);

      var ax = svg('text', { x: x(i), y: H - 7, class: 'hd-axis', 'text-anchor': 'middle' });
      ax.textContent = dayLabel(p.date);
      s.appendChild(ax);
    });

    // direct-label the endpoint only
    var last = pts[pts.length - 1];
    var lt = svg('text', {
      x: x(pts.length - 1), y: y(last.restingHeartRate) - 11, class: 'hd-val', 'text-anchor': 'end'
    });
    lt.textContent = last.restingHeartRate + ' bpm';
    s.appendChild(lt);
    return s;
  }

  /* ---------------- stat tiles ---------------- */
  function tile(label, value, unit, meterPct) {
    var t = el('div', 'hd-tile');
    t.appendChild(el('div', 'hd-tile-label', label));
    t.appendChild(el('div', 'hd-tile-value',
      value == null ? '--' : fmt(value) + (unit ? '<span class="hd-unit">' + unit + '</span>' : '')));
    if (meterPct != null) {
      var track = el('div', 'hd-meter');
      var fill = el('div', 'hd-meter-fill');
      fill.style.width = Math.max(0, Math.min(100, meterPct)) + '%';
      track.appendChild(fill);
      t.appendChild(track);
    }
    return t;
  }

  /* ---------------- table view (accessibility) ---------------- */
  function tableView(week) {
    var d = el('details', 'hd-table');
    d.appendChild(el('summary', null, 'View the numbers'));
    var rows = week.map(function (w) {
      return '<tr><th scope="row">' + dayLabel(w.date) + '</th><td>' + fmt(w.steps) +
             '</td><td>' + fmt(w.restingHeartRate) + '</td><td>' + fmt(w.bodyBattery) +
             '</td><td>' + (w.sleepHours == null ? '--' : w.sleepHours) + '</td></tr>';
    }).join('');
    d.appendChild(el('div', 'hd-table-scroll',
      '<table><thead><tr><th scope="col">Day</th><th scope="col">Steps</th>' +
      '<th scope="col">Resting HR</th><th scope="col">Body Battery</th>' +
      '<th scope="col">Sleep (h)</th></tr></thead><tbody>' + rows + '</tbody></table>'));
    return d;
  }

  function render(host, doc) {
    host.innerHTML = '';
    var t = doc.today || {};
    var week = doc.week || [];

    var kpi = el('div', 'hd-kpi');
    kpi.appendChild(tile('Steps', t.steps, null,
      doc.stepGoal ? (t.steps / doc.stepGoal) * 100 : null));
    kpi.appendChild(tile('Resting HR', t.restingHeartRate, ' bpm'));
    kpi.appendChild(tile('Body Battery', t.bodyBattery, '/100', t.bodyBattery));
    kpi.appendChild(tile('Sleep', t.sleepHours, ' h'));
    host.appendChild(kpi);

    var charts = el('div', 'hd-charts');
    var c1 = el('figure', 'hd-fig');
    c1.appendChild(el('figcaption', 'hd-cap', 'Steps, last 7 days' +
      (doc.stepGoal ? ' <span class="hd-ref">&middot; dashed line: ' +
        fmt(doc.stepGoal) + ' goal</span>' : '')));
    c1.appendChild(stepsChart(week, doc.stepGoal));
    charts.appendChild(c1);

    var hr = hrChart(week);
    if (hr) {
      var c2 = el('figure', 'hd-fig');
      c2.appendChild(el('figcaption', 'hd-cap', 'Resting heart rate, last 7 days'));
      c2.appendChild(hr);
      charts.appendChild(c2);
    }
    host.appendChild(charts);
    host.appendChild(tableView(week));

    var stamp = el('div', 'hd-stamp');
    var when = doc.updated ? new Date(doc.updated) : null;
    stamp.innerHTML = (doc.source === 'sample'
      ? 'Sample data &mdash; live Garmin sync not connected yet.'
      : 'Synced from Garmin Connect') +
      (when && !isNaN(when) ? ' &middot; updated ' + when.toLocaleDateString(undefined,
        { month: 'short', day: 'numeric', year: 'numeric' }) : '');
    host.appendChild(stamp);

    // hover layer
    var tip = makeTip(host);
    host.querySelectorAll('.hd-bar, .hd-dot').forEach(function (mark) {
      mark.addEventListener('mousemove', function (e) { tip.show(e, mark.dataset.label); });
      mark.addEventListener('mouseleave', tip.hide);
    });
  }

  document.addEventListener('DOMContentLoaded', function () {
    var host = document.getElementById('health-dashboard');
    if (!host) return;
    fetch('data/health.json', { cache: 'no-cache' })
      .then(function (r) {
        if (!r.ok) throw new Error(r.status);
        return r.json();
      })
      .then(function (doc) { render(host, doc); })
      .catch(function () {
        host.innerHTML = '<p class="hd-stamp">Health data is taking a nap &mdash; ' +
          'the daily sync will pick it back up.</p>';
      });
  });
})();
