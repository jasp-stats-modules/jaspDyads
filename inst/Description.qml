import QtQuick		2.11
import JASP.Module	1.0

Description
{
	name		: "jaspDyads"
	title		: qsTr("Dyads")
	description	: qsTr("This module offers dyadic network analyses.")
	icon		: "network.svg"
	version		: "0.19.6.0"
	author		: "Pankaj Singla"
	maintainer	: "Pankaj Singla <pankaj.r.singla@gmail.com>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Single Level")
		icon:	"single_level.svg"
	}

	Analysis
	{
		title:	qsTr("J2")
		qml:	"J2.qml"
		func:	"J2"
		requiresData: false
	}

	Analysis
	{
		title:	qsTr("P2")
		qml:	"P2.qml"
		func:	"P2"
		requiresData: false
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Multi Level")
		icon:	"multilevel.svg"
	}

	Analysis
	{
		title:	qsTr("J2ML")
		qml:	"J2ML.qml"
		func:	"J2ML"
		requiresData: false
	}

	Analysis
	{
		title:	qsTr("P2ML")
		qml:	"P2ML.qml"
		func:	"P2ML"
		requiresData: false
	}

	Analysis
	{
		title:	qsTr("B2ML")
		qml:	"B2ML.qml"
		func:	"B2ML"
		requiresData: false
	}
}