# The Harmonic Algorithm

![Header](img/header.png)

The Harmonic Algorithm, written in Haskell and R, generates musical domain 
specific data inside user defined constraints then filters it down and 
deterministically ranks it using a tailored Markov Chain model trained on 
ingested musical data. This presents a unique tool in the hands of the 
composer or performer which can be used as a writing aid, analysis 
device, for instrumental study or even in live performance. 

This open-source project is based on a long term research agenda that I've
pursued for many years, originating from an interest as an electric bass player
in performance and composition utilising the overtones of the instrument.

The Harmonic Algorithm takes the underpinning theoretical ideas from this 
research and realises them in a Command Line Interface. The Harmonic Algorithm
is not just useful for electric bass players, but can be utilised by 
composers and performers on or for any instrument.

The original research documents on which The Harmonic Algorithm draws it's
influence can be accessed at the following links:

Core document:
https://www.dropbox.com/s/7ctf91d59g2o558/The%20Harmonic%20Algorithm.pdf?dl=0

Reflective document:
https://www.dropbox.com/s/7ctf91d59g2o558/The%20Harmonic%20Algorithm.pdf?dl=0

All materials:
https://www.dropbox.com/s/2e2vb3ba0m91rfa/Harmonic%20Algorithm%20Reflections.pdf?dl=0

## Command Line Interface Installation Instructions
### Dependencies

1. Haskell Stack Tool (https://docs.haskellstack.org/en/latest/install_and_upgrade/)
2. R Interpreter (https://cran.r-project.org/)
3. TidyVerse packages for R (from inside R, execute the command 
`install.packages("tidyverse")`)

Once dependencies have been installed, the following steps can be used to
build the executable:

### Installation
1. Clone with git (recommended) or download and unzip the Harmonic Algorithm 
repository from GitHub. To clone with git, make sure that git is installed on 
your system and execute `git clone https://github.com/OscarSouth/theHarmonicAlgorithm` 
in the desired location.
2. In a terminal or command prompt, navigate to the Harmonic Algorithm directory.
3. Run the following command from inside the Harmonic Algorithm directory:
`stack --install-ghc build`

You can now run The Harmonic Algorithm by executing the following command from
inside it's directory:
`stack exec theHarmonicAlgorithm-exe`



