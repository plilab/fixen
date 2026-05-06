<a id="readme-top"></a>

<div align="center">
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="./assets/whiteonblack.png" width="60%">
  <source media="(prefers-color-scheme: light)" srcset="./assets/blackonwhite.png" width="60%">
  <img alt="Fixen Logo" src="./assets/blackonwhite.png">
</picture>
</div>


<br />
<div align="center">
<h3 align="center">The Fixen Domain-Specific Language</h3>

  <p align="center">
    Fixed-Point Computation for Haskell
    <br />
    <br />
    <a href="https://github.com/yonggqiii/fixen/issues/new?labels=bug&template=bug-report---.md">Report Bug</a>
    ·
    <a href="https://github.com/yonggqiii/fixen/issues/new?labels=enhancement&template=feature-request---.md">Request Feature</a>
  </p>
</div>

[![Haskell][haskell-shield]][haskell-badge-url]
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]


<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about">About</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installing-and-building">Installing and Building</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a>
      <ul>
        <li><a href="#basic-usage">Basic Usage</a></li>
        <li><a href="#exposing-unfoldings">Exposing Unfoldings</a></li>
        <li><a href="#plugin-options">Plugin Options</a></li>
      </ul>
    </li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About

The Fixen language is a Domain-Specific Language for expressing work-queue algorithms ergonomically. Work-queue algorithms are used to solve problems involving cycles and other types of self-reference, e.g., graphs, static analysis, automata minimization, type checking and distributed computing.

To write work-queue algorithms in Fixen, simply declare relations and rules. For instance, to write an algorithm for finding shortest paths to vertices in a graph, declare the following:

```
rel Edge: Vertex, Vertex, Dist
rel DistTo: Vertex, Dist

rule addDist: DistTo a d, Edge a b e |- DistTo b (d + e)
```

Then run the Fixen compiler on these declarations to generate Haskell source code that implements the work-queue algorithm that computes shortest paths. The generated source code is a Haskell module that can be imported and used by application code.

This library and executable has been tested on GHC version 9.8.4.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

This is an example of how you may give instructions on setting up your project locally.
To get a local copy up and running follow these simple example steps.

### Prerequisites
- [GHC](https://www.haskell.org/ghc/) v9.14.1 (base v4.22.0.0)
- [Cabal](https://www.haskell.org/cabal/) v3.16.0.0
- HLS v2.13.0.0
- git

### Installing and Building
* Clone this repository
  ```sh
  git clone https://github.com/yonggqiii/fixen.git
  cd fixen/
  ```
* Building
  ```sh
  cabal build
  ```
* Documentation
  ```sh
  cabal haddock
  ```
* Tests
  ```sh
  cabal test
  ```
You're all set!

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- ROADMAP -->
## Roadmap

- [x] Complete the parser
- [x] Complete the symbol solver
- [x] Generate rule forests IR
- [x] DB layout inference
- [ ] Generate Haskell source
- [ ] Benchmarks
- [ ] Unit tests

See the [open issues](https://github.com/yonggqiii/fixen-syb/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Top contributors:

<a href="https://github.com/yonggqiii/fixen/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=yonggqiii/fixen" alt="contrib.rocks image" />
</a>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Foo Yong Qi - yongqi@nus.edu.sg

Project Link: [https://github.com/yonggqiii/fixen](https://github.com/yonggqiii/fixen)

The Programming Languages Innovation Lab @ NUS: [https://github.com/plilab](https://github.com/plilab)

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/yonggqiii/fixen.svg?style=for-the-badge
[contributors-url]: https://github.com/yonggqiii/fixen/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/yonggqiii/fixen.svg?style=for-the-badge
[forks-url]: https://github.com/yonggqiii/fixen/network/members
[stars-shield]: https://img.shields.io/github/stars/yonggqiii/fixen.svg?style=for-the-badge
[stars-url]: https://github.com/yonggqiii/fixen/stargazers
[issues-shield]: https://img.shields.io/github/issues/yonggqiii/fixen.svg?style=for-the-badge
[issues-url]: https://github.com/yonggqiii/fixen/issues
[license-shield]: https://img.shields.io/github/license/yonggqiii/fixen.svg?style=for-the-badge
[license-url]: https://github.com/yonggqiii/fixen/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/fooyongqi
[haskell-shield]: https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white
[haskell-badge-url]: https://www.haskell.org
