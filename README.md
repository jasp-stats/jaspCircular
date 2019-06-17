# Project Title

This is a circular statistics module for JASP. It can be installed additionally to the JASP core module. It provides basic methods in the JASP GUI, such that analyses can be run without the need for programming. The underlying R code is based on the package [circular](https://CRAN.R-project.org/package=circular) by U. Lund et al..

### Prerequisites

An installed JASP version >= 0.10.

### Installing

Download the CircularStatistics.jaspMod directory to your local file system.
Open JASP
Click the + button in the main ribbon and select 'install module'
Select the downloaded CircularStatistics.jaspMod

That's it! The module and all it's R dependencies will be installed automatically.

You can now load a dataset in JASP as usual and start the analysis.

### How to use it
You can use the submenus of '''Circular Statistics''' to run descriptive analysis (such as plotting) or hypothesis tests. Most important is that you specify the period of your data for each analysis. This can be done in every submenu separately. A documentation of the functionalities can be found in the GUI itself. Just press the information button in the submenus. A toy data set to play with is the [ElNino.csv](examples/ElNino.csv). A commented example analysis for this data set can be found [here](examples/ElNino.jasp). To look at it, just open the ElNino.jasp file in JASP.

## Developed With

* [QtCreator](https://www.qt.io/)
* [RStudio](https://www.rstudio.com/)

## Contributing

If you would like to contribute to the module, you can use JASP as a development tool:
Download, fork, or clone the CircularStatistics repo.
Open JASP and go to preferences.
Go to Advanced.
Set the development path to the CircularStatistics repo.
Then click the + button in the main ribbon.
Click 'Install Development Module'.

There you go! Any changes made to the GUI or the R code will be reflected in JASP immediately. For further details we refer to the [JASP development documentation](https://github.com/jasp-stats/jasp-desktop/tree/development/Docs/development).

## Authors

* **Aaron Bahde**
See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

Special thanks go to the JASP team of the University of Amsterdam who supported this project at any time. The compatibility of this module with the JASP core is due to their refactoring of the JASP design.

The inspiration for this module was given by Philipp Berens and his publication [CircStat: A Matlab Toolbox for Circular Statistics](https://www.jstatsoft.org/article/view/v031i10).