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

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Theme 1.0

// All Analysis forms must be built with the From QML item
Form 
{
	usesJaspResults: true
	
	VariablesForm 
	{
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList { name: "variables";	title: qsTr("Variables"); allowedColumns: ["scale"] }
		AssignedVariablesList { name: "splitby";	title: qsTr("Split"); singleVariable: true; allowedColumns: ["ordinal", "nominal"] }
	}

    ColumnLayout
    {
        RadioButtonGroup
        {
            name: "periodGroup"
            title: qsTr("Period")
            RadioButton { value: "pi";	label: qsTr("π")	}
            RadioButton { value: "pi_2";	label: qsTr("2π")	}
            RadioButton
            {
                value: "custom";	label: qsTr("Custom:"); checked: true
                childrenOnSameRow: true
                DoubleField { name: "period"; defaultValue: 360; fieldWidth: 70; min: 0}
            }
        }

        Group
        {
            title: qsTr("Uniformity Tests")
            CheckBox
            {
                name: "rao"; label: qsTr("Rao's Spacing"); checked: true
                RadioButtonGroup
                {
                    name: "alphaRao"
                    indent: true
                    radioButtonsOnSameRow: true
                    RadioButton { value: "0.01";	label: qsTr("α = 0.01")	}
                    RadioButton { value: "0.05";	label: qsTr("α = 0.05")	}
                    RadioButton { value: "0.1";	label: qsTr("α = 0.1")	}
                }
            }
            CheckBox { name: "rayleigh";			label: qsTr("Rayleigh")}
            CheckBox
            {
                name: "modifiedRayleigh";			label: qsTr("V (modified Rayleigh)")
                DoubleField
                {
                 name: "testValue"; label: qsTr("Test value:"); fieldWidth: 70; max: 999999; min: 0.00001
                }
            }
        }

        Group
        {
            title: qsTr("Assumption Checks")
            CheckBox
            {
                name: "vonMisesCheck";	label: qsTr("Von Mises (Watson's goodness-of-fit)")
                RadioButtonGroup
                {
                    name: "alphaVonMises"
                    indent: true
                    radioButtonsOnSameRow: true
                    RadioButton { value: "0.01";	label: qsTr("α = 0.01")	}
                    RadioButton { value: "0.05";	label: qsTr("α = 0.05")	}
                    RadioButton { value: "0.1";	label: qsTr("α = 0.1")	}
                }
            }
        }
    }
}
