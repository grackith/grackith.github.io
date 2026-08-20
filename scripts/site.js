/* site.js — shared behavior for all pages.
   Replaces the gallery script that used to be pasted into every .Rmd. */
(function () {
  'use strict';

  /* ---------- intro box animation (runs once per session) ---------- */
  function initIntro() {
    if (sessionStorage.getItem('animationPlayed')) return;
    var box = document.querySelector('.intro-box');
    if (!box) return;
    box.classList.add('animate');
    sessionStorage.setItem('animationPlayed', 'true');
  }

  /* ---------- galleries: images and/or videos, dots + arrows ---------- */
  function Gallery(el) {
    this.el = el;
    this.figures = Array.prototype.slice.call(el.children).filter(function (n) {
      return n.tagName === 'FIGURE';
    });
    if (this.figures.length < 2) {
      if (this.figures.length === 1) this.show(0);
      return;
    }
    this.nav = this.buildNav();
    this.dots = Array.prototype.slice.call(this.nav.querySelectorAll('.gallery-dot'));
    this.i = 0;
    this.bind();
    this.show(0);
  }

  /* Reuse an authored .gallery-nav if present, otherwise build one.
     Either way the dot count is derived from the figures, so markup
     never has to hand-maintain a <span> per slide. */
  Gallery.prototype.buildNav = function () {
    var nav = this.el.nextElementSibling;
    if (!nav || !nav.classList.contains('gallery-nav')) {
      nav = document.createElement('div');
      nav.className = 'gallery-nav';
      this.el.parentNode.insertBefore(nav, this.el.nextSibling);
    }
    nav.innerHTML = '';
    var prev = document.createElement('button');
    prev.className = 'gallery-arrow gallery-prev';
    prev.type = 'button';
    prev.setAttribute('aria-label', 'Previous');
    prev.innerHTML = '&#8249;';
    nav.appendChild(prev);
    for (var n = 0; n < this.figures.length; n++) {
      var dot = document.createElement('span');
      dot.className = 'gallery-dot';
      dot.setAttribute('role', 'button');
      dot.setAttribute('tabindex', '0');
      dot.setAttribute('aria-label', 'Slide ' + (n + 1));
      nav.appendChild(dot);
    }
    var next = document.createElement('button');
    next.className = 'gallery-arrow gallery-next';
    next.type = 'button';
    next.setAttribute('aria-label', 'Next');
    next.innerHTML = '&#8250;';
    nav.appendChild(next);
    return nav;
  };

  Gallery.prototype.show = function (index) {
    var total = this.figures.length;
    index = (index + total) % total;
    this.figures.forEach(function (fig, n) {
      var on = n === index;
      fig.style.display = on ? 'block' : 'none';
      fig.classList.toggle('active', on);
      var vid = fig.querySelector('video');
      if (!vid) return;
      if (on) {
        // muted autoplay is the only kind browsers allow without a click
        var p = vid.play();
        if (p && p.catch) p.catch(function () {});
      } else {
        vid.pause();
        vid.currentTime = 0;
      }
    });
    if (this.dots) {
      this.dots.forEach(function (dot, n) {
        dot.classList.toggle('active', n === index);
      });
    }
    this.i = index;
  };

  Gallery.prototype.bind = function () {
    var self = this;
    this.dots.forEach(function (dot, n) {
      dot.addEventListener('click', function () { self.show(n); });
      dot.addEventListener('keydown', function (e) {
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); self.show(n); }
      });
    });
    var prev = this.nav.querySelector('.gallery-prev');
    var next = this.nav.querySelector('.gallery-next');
    if (prev) prev.addEventListener('click', function () { self.show(self.i - 1); });
    if (next) next.addEventListener('click', function () { self.show(self.i + 1); });

    var startX = 0;
    this.el.addEventListener('touchstart', function (e) {
      startX = e.touches[0].clientX;
    }, { passive: true });
    this.el.addEventListener('touchend', function (e) {
      var dx = startX - e.changedTouches[0].clientX;
      if (Math.abs(dx) > 50) self.show(self.i + (dx > 0 ? 1 : -1));
    }, { passive: true });
  };

  /* ---------- standalone autoplay videos ----------
     Marked .autoloop in the markup. Play only while on screen so a page
     with several clips doesn't decode all of them at once. */
  function initAutoloop() {
    var vids = document.querySelectorAll('video.autoloop');
    if (!vids.length) return;
    if (!('IntersectionObserver' in window)) {
      Array.prototype.forEach.call(vids, function (v) {
        var p = v.play(); if (p && p.catch) p.catch(function () {});
      });
      return;
    }
    var io = new IntersectionObserver(function (entries) {
      entries.forEach(function (entry) {
        var v = entry.target;
        if (entry.isIntersecting) {
          var p = v.play(); if (p && p.catch) p.catch(function () {});
        } else {
          v.pause();
        }
      });
    }, { threshold: 0.25 });
    Array.prototype.forEach.call(vids, function (v) { io.observe(v); });
  }

  document.addEventListener('DOMContentLoaded', function () {
    initIntro();
    document.querySelectorAll('.gallery-images').forEach(function (el) {
      new Gallery(el);
    });
    initAutoloop();
  });
})();
