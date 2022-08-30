import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:		"CircularStatisticsDescriptives"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

		ChangeRename { from: "splitby";					to: "splitVariable"							}
		ChangeRename { from: "plotVariables";			to: "distributionPlot"						}
		ChangeRename { from: "plotStacking";			to: "distributionPlotPointStack"			}
		ChangeRename { from: "plotMean";				to: "distributionPlotMeanVector"			}
		ChangeRename { from: "plotHistogram";			to: "distributionPlotHistogram"				}
		ChangeRename { from: "periodGroup";			    to: "periodOption"				            }
		ChangeRename { from: "period";			        to: "customPeriod"				            }

		ChangeJS
		{
			name:		"periodOption"
			jsFunction:	function(options)
			{
				switch(options["periodOption"])
				{
				case "pi_2":		            return "pi2";
				default:					    return options["periodOption"];
				}
			}
		}
	}

	Upgrade
	{
		functionName:		"CircularStatisticsOneSampleTests"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

		ChangeRename { from: "splitby";					to: "splitVariable"							}
		ChangeRename { from: "alphaVonMises";			to: "vonMisesCheckAlpha"					}
		ChangeRename { from: "periodGroup";			    to: "periodOption"				            }
		ChangeRename { from: "period";			        to: "customPeriod"				            }

		ChangeJS
		{
			name:		"periodOption"
			jsFunction:	function(options)
			{
				switch(options["periodOption"])
				{
				case "pi_2":		            return "pi2";
				default:					    return options["periodOption"];
				}
			}
		}
	}

	Upgrade
	{
		functionName:		"CircularStatisticsMultipleSampleTests"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

		ChangeRename { from: "periodGroup";			    to: "periodOption"				            }
		ChangeRename { from: "period";			        to: "customPeriod"				            }

		ChangeJS
		{
			name:		"periodOption"
			jsFunction:	function(options)
			{
				switch(options["periodOption"])
				{
				case "pi_2":		            return "pi2";
				default:					    return options["periodOption"];
				}
			}
		}
	}
}
