//
// Copyright (C) 2013-2018 University of Amsterdam
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
        AssignedVariablesList { name: "dependent";		title: qsTr("Dependent Variable");	allowedColumns: ["scale"]; singleVariable: true }
        AssignedVariablesList { name: "fixedFactors";	title: qsTr("Fixed Factors");		allowedColumns: ["ordinal", "nominal"]		}
	}

    RadioButtonGroup
    {
        name: "periodGroup"
        title: qsTr("Period")
        RadioButton { value: "pi";	text: qsTr("Pi")	}
        RadioButton { value: "pi_2";	text: qsTr("2Pi")	}
        RadioButton
        {
            value: "custom";	text: qsTr("Custom:"); checked: true
            childrenOnSameRow: true
            DoubleField { name: "period"; defaultValue: 360; fieldWidth: 50; max: 999999; min: 0.00001}
        }
    }

	Group
    {
        title: qsTr("One-way ANOVA")
        CheckBox { name: "watsonWilliams";			label: qsTr("Watson-Williams (parametric)")	}
        CheckBox { name: "watsonWheeler";			label: qsTr("Watson-Wheeler (non-parametric)")}
    }
	
	Group
		{
            title: qsTr("Two-way ANOVA")
            CheckBox { name: "harrisonKanji";	label: qsTr("Harrison-Kanji")}
        }		
}
