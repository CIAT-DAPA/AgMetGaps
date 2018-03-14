import glob, os

def convertToInt(crop):
    os.chdir("\\\\dapadfs\\Workspace_cluster_9\\AgMetGaps\\3_monthly_climate_variability\\katia_calendar")

    # Get and print a list of GRIDs from the workspace
    rasters = glob.glob('*Month.tif')

    for raster in rasters:
        intRaster = ster(raster.split(".")[0] + 'Int.tif')
        print intRaster
        os.system("C:\\OSGeo4W64\\bin\\gdal_translate.exe -ot UInt16 -of GTiff -a_nodata 'NA" + raster + " " + intRaster)

convertToInt("Maize")
convertToInt("Rice")
convertToInt("Wheat")