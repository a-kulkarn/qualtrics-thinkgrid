import pandas as pd

def calculate_quadrant_depth(dc, ac):
    # bottom left quadrant --> Free Depth
    # top left quadrant --> Sticky Depth
    # bottom right quadrant --> Directed Depth
    # top right quadrant --> AffDir Depth

    if dc < 4 and ac < 4:
        if (dc == 1 and ac == 1):
            depth = 5
        elif (dc == 1 and ac == 2) or (dc == 2 and ac == 1):
            depth = 4
        elif (dc == 1 and ac == 3) or (dc == 2 and ac == 2) or (dc == 3 and ac == 1):
            depth = 3
        elif (dc == 2 and ac == 3) or (dc == 3 and ac == 2):
            depth = 2
        elif (dc == 3 and ac == 3):
            depth = 1
        return depth, 0, 0, 0
    elif dc < 4 and ac >= 4:
        if (dc == 1 and ac == 6):
            depth = 5
        elif (dc == 1 and ac == 5) or (dc == 2 and ac == 6):
            depth = 4
        elif (dc == 1 and ac == 4) or (dc == 2 and ac == 5) or (dc == 3 and ac == 6):
            depth = 3
        elif (dc == 2 and ac == 4) or (dc == 3 and ac == 5):
            depth = 2
        elif (dc == 3 and ac == 4):
            depth = 1
        return 0, 0, 0, depth
    elif dc >= 4 and ac < 4:
        if (dc == 6 and ac == 1):
            depth = 5
        elif (dc == 5 and ac == 1) or (dc == 6 and ac == 2):
            depth = 4
        elif (dc == 4 and ac == 1) or (dc == 5 and ac == 2) or (dc == 6 and ac == 3):
            depth = 3
        elif (dc == 4 and ac == 2) or (dc == 5 and ac == 3):
            depth = 2
        elif (dc == 4 and ac == 3):
            depth = 1
        return 0, depth, 0, 0
    elif dc >= 4 and ac >= 4:
        if (dc == 6 and ac == 6):
            depth = 5
        elif (dc == 5 and ac == 6) or (dc == 6 and ac == 5):
            depth = 4
        elif (dc == 4 and ac == 6) or (dc == 5 and ac == 5) or (dc == 6 and ac == 4):
            depth = 3
        elif (dc == 4 and ac == 5) or (dc == 5 and ac == 4):
            depth = 2
        elif (dc == 4 and ac == 4):
            depth = 1
        return 0, 0, depth, 0
    

def extract_quadrant_depths(data_file, dc_column="Deliberate.Constraints", ac_column="Automatic.Constraints"):
    
    # data_file is a path to a CSV or Excel file containing the data
    if data_file.endswith(".csv"):
        data = pd.read_csv(data_file)
    elif data_file.endswith(".xlsx"):
        data = pd.read_excel(data_file)
    else:
        raise ValueError("Invalid file format. Please provide a CSV or Excel file.")
    
    # check if the required columns are present in the DataFrame
    if dc_column not in data.columns or ac_column not in data.columns:
        raise ValueError(f"DataFrame must contain columns: {dc_column} and {ac_column}.")
    
    dc = data[dc_column]
    ac = data[ac_column]
    free_depth, directed_depth, affdir_depth, sticky_depth = [], [], [], []
    # iterate over each row in output and calculate quadrant depth
    for dc_val, ac_val in zip(dc, ac):
        # check if dc_val and ac_val are not NaN or empty
        if pd.notna(dc_val) and pd.notna(ac_val) and dc_val != "" and ac_val != "":
            free, directed, affdir, sticky = calculate_quadrant_depth(int(dc_val), int(ac_val))
            free_depth.append(free)
            directed_depth.append(directed)
            affdir_depth.append(affdir)
            sticky_depth.append(sticky)
        else:
            free_depth.append(pd.NA)
            directed_depth.append(pd.NA)
            affdir_depth.append(pd.NA)
            sticky_depth.append(pd.NA)

    # Create a new DataFrame with the calculated depths
    depth_data = pd.DataFrame({
        "Free.Depth": free_depth,
        "Directed.Depth": directed_depth,
        "AffDir.Depth": affdir_depth,
        "Sticky.Depth": sticky_depth
    })
    # Concatenate the original data with the new depth data
    result = pd.concat([data, depth_data], axis=1)
    return result