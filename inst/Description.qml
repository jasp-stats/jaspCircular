import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspCircular"
	title		: qsTr("Circular Statistics")
	description	: qsTr("This module offers basic methods for directional data")
	version		: "0.16.4"
	author		: "Aaron Bahde, Philipp Berens (University of Tuebingen)"
	maintainer	: "Philipp Berens <philipp.berens@uni.tuebingen.de>"
	website		: "eye-tuebingen.de/berenslab/"
	license		: "GPL (>= 2)"
	icon		: "analysis-circstats-descriptives.svg"

	GroupTitle
	{
		title:	qsTr("Circular Descriptives")
		icon:	"analysis-circstats-descriptives.svg"
	}
	Analysis
	{
		title:	qsTr("Circular Descriptives")
		qml: "CircularStatisticsDescriptives.qml"
		func:	"CircularStatisticsDescriptives"
	}
	GroupTitle
	{
		title:	qsTr("Hypothesis Tests")
		icon:	"analysis-circstats-hypothesis-testing.svg"
	}
	Analysis
	{
		title:	qsTr("Circular One-Sample Tests")
		qml: "CircularStatisticsOneSampleTests.qml"
		func:	"CircularStatisticsOneSampleTests"
	}
	Analysis
	{
		title:	qsTr("Circular Multiple-Sample Tests")
		qml: "CircularStatisticsMultipleSampleTests.qml"
		func:	"CircularStatisticsMultipleSampleTests"
	}
}