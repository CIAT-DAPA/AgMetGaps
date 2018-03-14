# -*- coding: utf-8 -*-
# ----------------------------------------------------------------------------------------------------------------
# Created on: 2018-03-14
#
# Description: Calculate hotspot Richness
# ----------------------------------------------------------------------------------------------------------------

# Import modules
import arcpy

# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")

tmpDir = "D:\\ToBackup\\2018\\AgMetGapsProject\\tmpDir\\"

datasetsDir = "\\\\dapadfs\\Workspace_cluster_9\\AgMetGaps\\3_monthly_climate_variability\\"

# Local variables:
inTrueConstant = "1"
inFalseConstant = "0"
condition = "\"VALUE\" > 0.5"


def rastersThreshold(dataset, crop, model):
    # Set the current workspace
    if (model <> ""):
        arcpy.env.workspace = datasetsDir + dataset + "\\" + model + "\\" + crop
    else:
        arcpy.env.workspace = datasetsDir + dataset + "\\" + crop

    # Get and print a list of GRIDs from the workspace
    inConditionalRasters = arcpy.ListRasters("*", "TIF")

    for inConditionalRaster in inConditionalRasters:
        # outRaster = inConditionalRaster.split(".")[0] + "_con.tif"
        outRaster = tmpDir + inConditionalRaster.split(".")[0] + "_" + dataset[:4] + "_" + crop[:4] + "_" + model[:3] + ".tif"
        print inConditionalRaster + "   " + outRaster

        # Process: Con
        arcpy.gp.Con_sa(inConditionalRaster, inTrueConstant, outRaster, inFalseConstant, condition)


for dataset in ["correlation", "GROC", "models"]:
    for crop in ["Maize", "Rice", "Wheat"]:
        if (dataset == "models"):
            # for model in ["polynomial", "GAM"]:
            for model in ["polynomial"]:
                print "\n>>> Dataset > " + dataset + " --- Crop > " + crop + " --- Model > " + model
                rastersThreshold(dataset, crop, model)
        else:
            print "\n>>> Dataset > " + dataset + " --- Crop > " + crop
            rastersThreshold(dataset, crop, "")

