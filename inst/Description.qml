import QtQuick
import JASP.Module

Description
{
	title		: qsTr("Circular Statistics")
	description	: qsTr("This module offers basic methods for directional data")
	icon		: "analysis-circstats-descriptives.svg"
	hasWrappers	: true
	
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
