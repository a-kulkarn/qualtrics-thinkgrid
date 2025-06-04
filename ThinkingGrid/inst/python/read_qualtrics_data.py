import copy
import pandas as pd

def subset_qualtrics(data, identifier):
    identifierStart = "UQID{}".format(identifier)
    data = data.filter(regex=identifierStart)
    colNames = data.columns
    # split the column names on identifierStart
    data.columns = [col.split(identifierStart)[1] for col in colNames]
    return data


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

def read_qualtrics_data(outputFileDir, setup):
    # outputFileDir -- exported survery data from qualtrics.
    # setup -- setup file used to generate the survey. (Or possibly the list of identifiers.)

    # check if setup is an instance of list or tuple
    if isinstance(setup, str):
        # check if setup is csv or xlsx
        if setup.endswith(".csv") or setup.endswith(".xlsx"):
            setup = pd.read_csv(setup) if setup.endswith(".csv") else pd.read_excel(setup)
            setup = setup["id"].to_list()
    elif isinstance(setup, (list, tuple)):
        if len(setup) != len(set(setup)):
            raise ValueError("All elements in the list or tuple should be unique.")
        setup = setup
    else:
        raise ValueError("Invalid setup format. Please use csv, xlsx, list or tuple.")

    # check if outputFile is csv or xlsx
    if outputFileDir.endswith(".csv") or outputFileDir.endswith(".xlsx"):
        output = (
            pd.read_csv(outputFileDir, header=None)
            if outputFileDir.endswith(".csv")
            else pd.read_excel(outputFileDir, header=None)
        )
    else:
        raise ValueError(
            "Invalid file format for the qualtrics file. Please use csv or xlsx file format."
        )

    # delete first and third row of the output file
    output = output.drop([0, 2])
    output = output.reset_index(drop=True)
    # set the first row as the header
    output.columns = output.iloc[0]
    output = output.drop([0])
    output = output.reset_index(drop=True)

    # iterate over each row in output
    uid, probeid, dc, ac = [], [], [], []
    uidLocal = 1
    for index, row in output.iterrows():
        for identifier in setup:
            tempRow = row
            tempRow = pd.DataFrame(tempRow).T
            tempRow.columns = [str(col) for col in output.columns]
            qSubset = subset_qualtrics(copy.deepcopy(tempRow), identifier)
            temptemp = qSubset[qSubset == "On"]
            temp = (
                temptemp.dropna(axis=1, how="all").columns[0]
                if not temptemp.dropna(axis=1, how="all").empty
                else "nan"
            )
            if temp != "nan":
                uid.append(uidLocal)
                probeid.append(identifier)
                if temp[1] == "n" or temp[1] == "N":
                    dc.append("N/A")
                    ac.append("N/A")
                else:
                    dc.append(temp[1])
                    ac.append(temp[2])
            else:
                uid.append(uidLocal)
                probeid.append(identifier)
                dc.append("not recorded")
                ac.append("not recorded")
        uidLocal += 1

    free_depth, directed_depth, affdir_depth, sticky_depth = [], [], [], []

    # iterate over each row in output and calculate quadrant depth
    for dc_val, ac_val in zip(dc, ac):
        if dc_val != "N/A" and ac_val != "N/A":
            free, directed, affdir, sticky = calculate_quadrant_depth(int(dc_val), int(ac_val))
            free_depth.append(free)
            directed_depth.append(directed)
            affdir_depth.append(affdir)
            sticky_depth.append(sticky)

        output = pd.DataFrame(
        list(zip(uid, probeid, dc, ac, free_depth, directed_depth, affdir_depth, sticky_depth)),
        columns=["uid", "Probe.Identifier", "Deliberate.Constraints", "Automatic.Constraints", "Free.Depth", "Directed.Depth", "AffDir.Depth", "Sticky.Depth"],
    )

    for col in ["Deliberate.Constraints", "Automatic.Constraints", "Free.Depth", "Directed.Depth", "AffDir.Depth", "Sticky.Depth"]:
            output[col] = output[col].astype(int)

    # Reorder columns.
    output = output[
        [
            "uid",
            "Probe.Identifier",
            "Deliberate.Constraints",
            "Automatic.Constraints",
            "Free.Depth",
            "Directed.Depth",
            "AffDir.Depth",
            "Sticky.Depth",
        ]
    ]

    # Return.
    return output


################################################################################
#
# Main driver.
#
################################################################################

# if __name__ == "__main__":
#     setup_file = "test_data/testQuestion.csv"
#     data_file = "test_data/testQuestionOutput.csv"    
#     res = read_qualtrics_data(data_file, setup_file)
#     print(res)
