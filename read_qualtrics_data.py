import copy
import pandas as pd

def subset_qualtrics(data, identifier):
    identifierStart = "UQID{}".format(identifier)
    data = data.filter(regex=identifierStart)
    colNames = data.columns
    # split the column names on identifierStart
    data.columns = [col.split(identifierStart)[1] for col in colNames]
    return data


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

    output = pd.DataFrame(
        list(zip(uid, probeid, dc, ac)),
        columns=["uid", "Probe Identifier", "Deliberate Constraints", "Automatic Constraints"],
    )
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
