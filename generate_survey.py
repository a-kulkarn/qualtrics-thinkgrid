################################################################################
#
#     generate_survey.py
#
# Code meant to automatically generate qualtrics survey questions from a
# csv file. Meant for use in some project of Zac's.
#
################################################################################

import pandas as pd
import json
import copy

# In order to add a thing to the qualtrics survey file, it appears to need updating in
# three potential locations
#
# 1. The question needs to appear in the "SurveyElements" field.
# 2. The "SurveyBlocks" needs to be updated to add/modify a block for the question to
#    appear.
# 3. The "SurveyFlow" needs to be updated for <Insert Reason Here>.
#    -- Hypothesis: Probably has something to do with how the survey executes.


################################################################################
#
# Read / Write
#
################################################################################

def read_json_file(fname):
    with open(fname, 'r') as F:
        X = json.load(F)
    return X

################################################################################
#
# Constants
#
################################################################################

const_survey_template   = read_json_file("survey-template.json")
const_block_template    = read_json_file("block-template.json")
const_question_template = read_json_file("question-template.json")

################################################################################
#
# Squares
#
################################################################################

import numpy as np
import skimage as ski

def square_position(i, j, width = 50, gap = 25, origin_coordinates = [200, 420]):
    """
    Given the square with index (i,j) in the grid, determine
    the (x,y)-coordinates of the principal corner.
    """
    init = np.array(origin_coordinates, dtype=int)
    d = width + gap
    return init + np.array([i * d, -j * d], dtype=int)


def correct_squares(gap = 25, width = 25, height = 25, origin_coordinates = [200, 420],
                    generate_image_file = False):

    regions = const_question_template["Payload"]["Regions"]

    dx = np.array([width,  0], dtype=int)
    dy = np.array([0, height], dtype=int)
    
    for R in regions:
        # Qualtrics labels are 1-indexed.
        desc = R["Description"]
        if desc == "s1NA": continue

        i = int(desc[2]) - 1
        j = int(desc[3]) - 1

        # Do math
        v = square_position(i, j,
                            width = width,
                            gap = gap,
                            origin_coordinates=origin_coordinates)
        
        coords = [v, v + dx, v + dx + dy, v + dy]

        # Assign values. Note we have to type cast back to python.
        R["Height"] = int(width)
        R["Width"]  = int(height)
        R["X"]      = int(v[0])
        R["Y"]      = int(v[1])

        convert = lambda v : {'X' : int(v[0]), 'Y' : int(v[1])}
        R["Shapes"] = [list(map(convert,  coords))]


    # If we need to update the image file, do that.
    if generate_image_file:
        draw_white_squares(regions)
        
    return

def draw_white_squares(regions):
    white = [255, 255, 255, 255]
    
    img = ski.io.imread("Thinking-grid-template.png")

    for R in regions:
        desc = R["Description"]
        if desc == "s1NA": continue
        
        a = R["X"]
        b = R["Y"]
        for x in range(a, a+R["Width"]):
            for y in range(b, b+R["Height"]):
                img[y,x] = white

    # Draw the N/A square.
    draw_NA_square(img, R)
    
    # write to file
    ski.io.imsave("Thinking-grid-generated-squares.png", img)
    return


import cv2
def draw_NA_square(img, R):
    font = cv2.FONT_HERSHEY_SIMPLEX
    black = (0,0,0,255)

    x = R["X"]
    y = R["Y"]
    w = R["Width"]
    d = int(w/2) 
    cv2.putText(img, 'N/A', (x + d - 30, y + d),  font,  1, black, 2)
    
    

################################################################################
#
# Main functionality
#
################################################################################

def question_text(row):
    return row['thoughts']

def participant_id(row):
    return row['id']

def updated_square_string(participant_id, string):
    return "ID{}_{}".format(participant_id, string[2:4])

def question_from_row(row, question_id, question_export_tag):
    Q = copy.deepcopy(const_question_template)

    # Add question text.
    text = question_text(row)
    pid = participant_id(row)

    if not isinstance(text, str):
        text = "NO RESPONSE RECORDED."
    
    # Assign payload attributes.
    Q["PrimaryAttribute"] = question_id
    Q["Payload"]["QuestionID"] = question_id
    Q["Payload"]["QuestionDescription"] = "" 
    Q["Payload"]["QuestionText"] = text
    Q["Payload"]["DataExportTag"] = question_export_tag

    #####
    # Update region names and identifiers to include ID.
    choices = Q["Payload"]["Choices"]
    for k, C in choices.items():
        C["Display"] = updated_square_string(pid, C["Display"])

    regions = Q["Payload"]["Regions"]
    for R in regions:
        R["Description"] = updated_square_string(pid, R["Description"])
    
    return Q


def add_question_to_survey(survey, question, block_id, participant_id):

    # Add question to survey pile.
    survey["SurveyElements"].append(question)

    # Create a new block to harbor the question.
    new_block = copy.deepcopy(const_block_template)
    
    new_block_entry = {
      "Type": "Question",
      "QuestionID": Q["Payload"]["QuestionID"]
    }

    new_block["ID"] = block_id
    new_block["Description"] = "Block_{}".format(participant_id)
    new_block["BlockElements"].append(new_block_entry)

    # Update the blocks to include the question.
    blocks = survey["SurveyElements"][0]["Payload"]
    blocks[len(blocks)+1] = new_block

    # Update the Flow data to include the new block
    base_flow_count = 25

    flow_payload = survey["SurveyElements"][1]["Payload"]
    flow = flow_payload["Flow"]
    block_randomizer = flow[0]
    block_randomizer_flow = block_randomizer["Flow"]
    new_flow_count = base_flow_count + len(block_randomizer_flow)

    
    new_flow_entry = {
        "Type": "Block",
        "ID": block_id,
        "FlowID": "FL_{}".format(new_flow_count),
        "Autofill": []
    }

    block_randomizer_flow.append(new_flow_entry)
    block_randomizer["SubSet"] += 1
    
    # NOTE: Might need to update the "Properties" field?
    flow_payload["Properties"]["Count"] = new_flow_count
    return

################################################################################
#
# Main script.
#
################################################################################

import sys

if __name__ == "__main__":

    # <COMMENT FOR ZAC> This is where you adjust parameters to generate the squares.
    gap    = 25
    width  = 50
    height = width
    origin_coordinates = [200, 420]
 
    # <COMMENT FOR ZAC> If you regenerate the squares, make sure to turn this on to
    # create the image file. You'll then have to upload the image into qualtrics.
    #
    # After that, you need to pull the image ID on qualtrics and copy-paste that into
    # the "GraphicID" field in the `question-template.json` file.
    generate_image_file = False    


    # <COMMENT FOR ZAC> Change this to choose which questions are filtered into the survey.
    def include_question_in_survey(csv_row):
        """
        Given a row of a csv table, return a boolean value on whether to include the
        question into the survey. 
        """
        # Alias
        row = csv_row

        # You can filter on the fields in row, for example
        # if row["thoughts"] == np.nan: return False

        # Default is to include all questions.
        return True
    

    # <COMMENT FOR ZAC> This parameter controls the max survey size.
    # once the counter ticks over this value, an additional survey file
    # is generated to hold the next batch.
    max_survey_size = None


    ########################################

    
    # Determine correct squares and adjust the template.
    correct_squares(gap    = gap,
                    width  = width,
                    height = height,
                    origin_coordinates = origin_coordinates,
                    generate_image_file = generate_image_file)
    
    # Read the data file
    data_table = pd.read_csv("Datafile_nsf_pilot.csv")

    # Create the survey object
    survey = copy.deepcopy(const_survey_template)

    # Parse command line option
    if len(sys.argv) == 1:
        LIMIT = 5
    else:
        LIMIT = int(sys.argv[1])

    num_questions_included = 0
    partition_number = 0
    for i, row in data_table.iterrows():
        
        if i >= LIMIT: break

        if max_survey_size and num_questions_included >= max_survey_size:

            # Write current batch
            with open("output-survey-{}.qsf".format(partition_number), "w") as F:
                json.dump(survey, F)

            # Reset counters and survey object.
            survey = copy.deepcopy(const_survey_template)
            num_questions_included = 0
            partition_number += 1


        if include_question_in_survey(row):
            pid = participant_id(row)
            Q = question_from_row(row, "QID{}".format(1000 + i), "ID_{}".format(row["id"]))
            add_question_to_survey(survey, Q, "BL_MamboNo{}".format(i), pid)

            # Update counter.
            num_questions_included += 1
            
    # Write file
    with open("output-survey-{}.qsf".format(partition_number), "w") as F:
        json.dump(survey, F)

