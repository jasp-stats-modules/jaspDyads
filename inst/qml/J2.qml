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
	Group
	{
		FileSelector
		{
			id:										net
			name:									"net"
			label:									qsTr("Net")
			placeholderText:						qsTr("path/network_data.xlsx")
			filter:									"*.xlsx"
			save:									false
			fieldWidth:								300 * preferencesModel.uiScale
			value: 									libPathDir.value === "" ? "" : (libPathDir.value + "/jaspDyads/data/network.xlsx")
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
			value: 									libPathDir.value === "" ? "" : (libPathDir.value + "/jaspDyads/data/sender_file1.xlsx")
			multiple:								false
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
			value: 									libPathDir.value === "" ? "" : (libPathDir.value + "/jaspDyads/data/reciever_file1.xlsx")
			multiple:								false
		}

		FileSelector
		{
			id:										density
			name:									"density"
			label:									qsTr("Density")
			placeholderText:						qsTr("path/density_data.xlsx")
			filter:									"*.xlsx"
			save:									false
			fieldWidth:								300 * preferencesModel.uiScale
			value: 									libPathDir.value === "" ? "" : (libPathDir.value + "/jaspDyads/data/density_file1.xlsx")
			multiple:								false
		}

		FileSelector
		{
			id:										reciprocity
			name:									"reciprocity"
			label:									qsTr("Reciprocity")
			placeholderText:						qsTr("path/reciprocity_data.xlsx")
			filter:									"*.xlsx"
			save:									false
			fieldWidth:								300 * preferencesModel.uiScale
			value: 									libPathDir.value === "" ? "" : (libPathDir.value + "/jaspDyads/data/reciprocity_file1.xlsx")
			multiple:								false
		}
	}

	Group
	{
		IntegerField {name: "burnin"; label: qsTr("Burnin"); defaultValue: 10; min: 0; placeholderText: qsTr("10000")}
		IntegerField {name: "sample"; label: qsTr("Sample"); defaultValue: 40; min: 0; placeholderText: qsTr("80000")}
		IntegerField { name: "adapt"; label: qsTr("Adapt"); defaultValue: 10; min: 0; placeholderText: qsTr("100")}
		IntegerField {name: "seed"; label: qsTr("Seed"); defaultValue: 1; min: 0; placeholderText: qsTr("1")}
	}

	Group
	{
		CheckBox {name: "center"; label: qsTr("Center"); checked: false}
	}

	Button
	{
		label: "Compute"
		CheckBox {id: compute; name: "compute"; checked: true; visible: false}
		onClicked: compute.click()
	}

	// get the module location
	DropDown
	{
		id: libPathDir
		name: "libPathLocation"
		visible: false
		rSource: "libPathDir"
	}
}