const sections = document.querySelectorAll(".section, .hero-card, .graph-box, .card");

const observer = new IntersectionObserver(
  entries => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        entry.target.classList.add("show");
      }
    });
  },
  { threshold: 0.12 }
);

sections.forEach(section => {
  section.classList.add("hidden");
  observer.observe(section);
});