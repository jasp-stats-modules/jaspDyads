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
		placeholderText:						qsTr("path/network_data.xlsx")
		filter:									"*.xlsx"
		save:									false
		fieldWidth:								300 * preferencesModel.uiScale
		defaultValue:							"C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/network.xlsx"
		directory:								false
	}

	FileSelector
	{
		id:										sender
		name:									"sender"
		label:									qsTr("Sender")
		placeholderText:						qsTr("path/")
		filter:									"*.xlsx"
		save:									false
		fieldWidth:								300 * preferencesModel.uiScale
		defaultValue:							"C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/sender_file1.xlsx;C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/sender_file2.xlsx"
		multiple:								true
	}

	FileSelector
	{
		id:										receiver
		name:									"receiver"
		label:									qsTr("Receiver")
		placeholderText:						qsTr("path/")
		filter:									"*.xlsx"
		save:									false
		fieldWidth:								300 * preferencesModel.uiScale
		defaultValue:							"C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/reciever_file1.xlsx;C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/receiver_file2.xlsx"
		multiple:								true
	}

	FileSelector
	{
		id:										density
		name:									"density"
		label:									qsTr("Density")
		placeholderText:						qsTr("path/")
		filter:									"*.xlsx"
		save:									false
		fieldWidth:								300 * preferencesModel.uiScale
		defaultValue:							"C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/density_file1.xlsx;C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/density_file2.xlsx"
		multiple:								true
	}

	FileSelector
	{
		id:										reciprocity
		name:									"reciprocity"
		label:									qsTr("Reciprocity")
		placeholderText:						qsTr("path/")
		filter:									"*.xlsx"
		save:									false
		fieldWidth:								300 * preferencesModel.uiScale
		defaultValue:							"C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/reciprocity_file1.xlsx;C:/Users/panka/OneDrive/Desktop/My_Stuff/Amsterdam/Project/JASP/jaspDyads/tests/testthat/data/reciprocity_file2.xlsx"
		multiple:								true
	}

	IntegerField {name: "burnin"; label: qsTr("Burnin"); defaultValue: 15; min: 0; placeholderText: qsTr("10000")}
	IntegerField {name: "adapt"; label: qsTr("Adapt"); defaultValue: 15; min: 0; placeholderText: qsTr("100")}
	IntegerField {name: "seed"; label: qsTr("Seed"); defaultValue: 1; min: 0; placeholderText: qsTr("1")}
	CheckBox {name: "center"; label: qsTr("Center"); checked: true}
	CheckBox {name: "separate"; label: qsTr("Separate"); checked: false}
}