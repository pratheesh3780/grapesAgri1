---
  title: 'Gala: A Python package for galactic dynamics'
tags:
  - Python
- astronomy
- dynamics
- galactic dynamics
- milky way
authors:
  - name: Adrian M. Price-Whelan^[co-first author] # note this makes a footnote saying 'co-first author'
orcid: 0000-0003-0872-7098
affiliation: "1, 2" # (Multiple affiliations must be quoted)
- name: Author Without ORCID^[co-first author] # note this makes a footnote saying 'co-first author'
affiliation: 2
- name: Author with no affiliation^[corresponding author]
affiliation: 3
affiliations:
  - name: Lyman Spitzer, Jr. Fellow, Princeton University
index: 1
- name: Institution Name
index: 2
- name: Independent Researcher
index: 3
date: 13 August 2017
bibliography: paper.bib

---
  
# Summary
  
  The forces on stars, galaxies, and dark matter under external gravitational
fields lead to the dynamical evolution of structures in the universe. The orbits
of these bodies are therefore key to understanding the formation, history, and
future state of galaxies. The field of "galactic dynamics," which aims to model
the gravitating components of galaxies to study their structure and evolution,
is now well-established, commonly taught, and frequently used in astronomy.
Aside from toy problems and demonstrations, the majority of problems require
efficient numerical tools, many of which require the same base code (e.g., for
                                                                     performing numerical orbit integration).

# Statement of need

`Gala` is an Astropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing
flexibility or ease-of-use in the user-interface. The API for `Gala` was
designed to provide a class-based and user-friendly i
                                                          
# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$
  
  Double dollars make self-standing equations:
  
  $$\Theta(x) = \left\{\begin{array}{l}
    0\textrm{ if } x < 0\cr
    1\textrm{ else}
    \end{array}\right.$$
      
      You can also use plain \LaTeX for equations
    \begin{equation}\label{eq:fourier}
    \hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
    \end{equation}
    and refer to \autoref{eq:fourier} from text.
    
   
# Acknowledgements
    
    We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
    Oh, and support from Kathryn Johnston during the genesis of this project.
    
# References
