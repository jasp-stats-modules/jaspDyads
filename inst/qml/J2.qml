//
// Copyright (C) 2013-2019 University of Amsterdam
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
import JASP.Controls
import JASP 1.0

Form
{
	columns: 1
	FileSelector
	{
		id:										net
		name:									"net"
		label:									qsTr("Net")
		placeholderText:						qsTr("path/network_data_file.csv")
		filter:									"*.csv"
		save:									false
		fieldWidth:								300 * preferencesModel.uiScale
		defaultValue:							"C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/net.csv"
	}

	TextArea
	{
		title:									qsTr("Comma-separated Sequence of Sender Covariates")
		height:									50
		name:									"sender"
		textType:								JASP.TextTypeSource
		separators:								[",",";","\n"]
		placeholderText: 						qsTr("1,0,1,0,1,1,0,1,0,1")
		text: 									"1,0,1,0,1,1,0,1,0,1"
	}

	TextArea
	{
		title:									qsTr("Comma-separated Sequence of Receiver Covariates")
		height:									50
		name:									"receiver"
		textType:								JASP.TextTypeSource
		separators:								[",",";","\n"]
		placeholderText: 						qsTr("0,1,0,1,0,0,1,0,1,0")
		text: 									"0,1,0,1,0,0,1,0,1,0"
	}

	FileSelector
	{
		id:										density
		name:									"density"
		label:									qsTr("Density")
		placeholderText:						qsTr("path/density_data.csv")
		filter:									"*.csv"
		save:									false
		fieldWidth:								300 * preferencesModel.uiScale
		defaultValue:							"C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/density_p2.csv"
	}

	FileSelector
	{
		id:										reciprocity
		name:									"reciprocity"
		label:									qsTr("Reciprocity")
		placeholderText:						qsTr("path/reciprocity_data.csv")
		filter:									"*.csv"
		save:									false
		fieldWidth:								300 * preferencesModel.uiScale
		defaultValue:							"C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/reciprocity_p2.csv"
	}

	IntegerField {name: "burnin"; label: qsTr("Burnin"); defaultValue: 10; min: 0; placeholderText: qsTr("10000")}
	IntegerField {name: "sample"; label: qsTr("Sample"); defaultValue: 40; min: 0; placeholderText: qsTr("40000")}
	IntegerField { name: "adapt"; label: qsTr("Adapt"); defaultValue: 10; min: 0; placeholderText: qsTr("100")}
	IntegerField {name: "seed"; label: qsTr("Seed"); defaultValue: 1; min: 0; placeholderText: qsTr("1")}
	CheckBox {name: "center"; label: qsTr("Center"); checked: false}
}