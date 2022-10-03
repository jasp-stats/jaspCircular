import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:		"CircularStatisticsDescriptives"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "splitby";					to: "splitVariable"							}
		ChangeRename { from: "plotVariables";			to: "distributionPlot"						}
		ChangeRename { from: "plotStacking";			to: "distributionPlotPointStack"			}
		ChangeRename { from: "plotMean";				to: "distributionPlotMeanVector"			}
		ChangeRename { from: "plotHistogram";			to: "distributionPlotHistogram"				}
		ChangeRename { from: "period";	                to: "customPeriod"                          }
		ChangeRename { from: "periodGroup";	            to: "period"                                }

		ChangeJS
		{
			name:		"period"
			jsFunction:	function(options)
			{
				switch(options["period"])
				{
				case "pi_2":		            return "pi2";
				default:					    return options["period"];
				}
			}
		}
	}

	Upgrade
	{
		functionName:		"CircularStatisticsOneSampleTests"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "splitby";					to: "splitVariable"							}
		ChangeRename { from: "alphaVonMises";			to: "vonMisesCheckAlpha"					}
		ChangeRename { from: "period";	                to: "customPeriod"                          }
		ChangeRename { from: "periodGroup";	            to: "period"                                }
		ChangeRename { from: "alphaRao";			    to: "raoAlpha"				                }

		ChangeJS
		{
			name:		"period"
			jsFunction:	function(options)
			{
				switch(options["period"])
				{
				case "pi_2":		            return "pi2";
				default:					    return options["period"];
				}
			}
		}
	}

	Upgrade
	{
		functionName:		"CircularStatisticsMultipleSampleTests"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "period";	to: "customPeriod" }
		ChangeRename { from: "periodGroup";	to: "period" }

		ChangeJS
		{
			name:		"period"
			jsFunction:	function(options)
			{
				switch(options["period"])
				{
				case "pi_2":		            return "pi2";
				default:					    return options["period"];
				}
			}
		}
	}
}
