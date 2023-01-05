//
// Copyright (C) 2019 Aaron Bahde and Philipp Berens, University of Tuebingen
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

import "./common" as Circular

// All Analysis forms must be built with the From QML item
Form 
{
	VariablesForm 
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "variables";	title: qsTr("Variables"); allowedColumns: ["scale"] }
		AssignedVariablesList { name: "splitVariable";	title: qsTr("Split"); singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
	}

	Circular.Period {}
	
	Section
	{
		title: qsTr("Plots")
		Group
		{
			CheckBox { name: "distributionPlot"; label: qsTr("Distribution plots"); id: distributionPlot }
			CheckBox { name: "distributionPlotPointStack"; label: qsTr("Stack points"); enabled: distributionPlot.checked; indent: true	}
			CheckBox { name: "distributionPlotMeanVector"; label: qsTr("Display mean vector")	; enabled: distributionPlot.checked; indent: true}
			CheckBox { name: "distributionPlotHistogram"; label: qsTr("Display histogram"); enabled: distributionPlot.checked; indent: true}
		}
	}

	
	Section
	{
		title: qsTr("Statistics")
		Group
		{
			title: qsTr("Central Tendency")
			CheckBox { name: "meanDirection";	text: qsTr("Mean direction");		checked: true	}
			CheckBox { name: "meanLength";		text: qsTr("Mean resultant length");}
			CheckBox { name: "median";			text: qsTr("Median");				}
		}

		Group
		{
			title: qsTr("Dispersion")
			CheckBox { name: "standardDeviation";	text: qsTr("Std. deviation"); checked: true	}
			CheckBox { name: "variance";			text: qsTr("Variance")						}
			CheckBox { name: "range";				text: qsTr("Range")							}
		}
	}
}
