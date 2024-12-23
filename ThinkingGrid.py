import pandas as pd
import json
import copy
import numpy as np
import skimage as ski
import cv2

class ThinkingGrid:

    #region init

    def read_json_file(self, fname):
        with open(fname, 'r') as F:
            X = json.load(F)
        return X

    def __init__(self, setupFile, questionText = True, draw_NA_square = False, output_file = "output-survey"):
        self.const_survey_template   = self.read_json_file("json/survey-template.json")
        self.const_block_template    = self.read_json_file("json/block-template.json")
        self.const_question_templateNA = self.read_json_file("json/question-template.json")
        self.const_question_templateNoNA = self.read_json_file("json/question-template-noNA.json")
        self.setupFile = setupFile
        # check if setupFile is csv or xlsx
        if setupFile.endswith('.csv') or setupFile.endswith('.xlsx'):
            self.setup = pd.read_csv(setupFile) if setupFile.endswith('.csv') else pd.read_excel(setupFile)
        else:
            raise ValueError("Invalid file format. Please use csv or xlsx file format.")
        if draw_NA_square:
            self.const_question_template = self.const_question_templateNA
        else:
            self.const_question_template = self.const_question_templateNoNA
        self.questionText = questionText
        self.output_file = output_file

    #endregion

    #region square functions
    def square_position(self, i, j, width = 50, gap = 25, origin_coordinates = [200, 420]):
        """
        Given the square with index (i,j) in the grid, determine
        the (x,y)-coordinates of the principal corner.
        """
        init = np.array(origin_coordinates, dtype=int)
        d = width + gap
        return init + np.array([i * d, -j * d], dtype=int)
    
    def draw_NA_square(self, img, R): # only call this function if you want to draw the N/A square
        font = cv2.FONT_HERSHEY_SIMPLEX
        black = (0,0,0,255)

        x = R["X"]
        y = R["Y"]
        w = R["Width"]
        d = int(w/2) 
        cv2.putText(img, 'N/A', (x + d - 30, y + d),  font,  1, black, 2)

    def draw_white_squares(self, regions):
        white = [255, 255, 255, 255]
        
        img = ski.io.imread("Thinking-grid-template.png")

        for R in regions:
            desc = R["Description"]
            if desc == "s1NA":
                continue
            
            a = R["X"]
            b = R["Y"]
            for x in range(a, a+R["Width"]):
                for y in range(b, b+R["Height"]):
                    img[y,x] = white

        # if self.NASquareBinary:
        #     self.draw_NA_square(img, R)
        
        # write to file
        ski.io.imsave("Thinking-grid-generated-squares.png", img)
        return

    def correct_squares(self, gap = 25, width = 25, height = 25, origin_coordinates = [200, 420],
                    generate_image_file = False):

        regions = self.const_question_template["Payload"]["Regions"]

        dx = np.array([width,  0], dtype=int)
        dy = np.array([0, height], dtype=int)
        
        for R in regions:
            # Qualtrics labels are 1-indexed.
            desc = R["Description"]
            if desc == "s1NA": 
                continue

            i = int(desc[2]) - 1
            j = int(desc[3]) - 1

            # Do math
            v = self.square_position(i, j,
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
            self.draw_white_squares(regions)
            
        return
    
    #endregion

    #region main functions
    def question_text(self, row):
        return row['question']

    def participant_id(self, row):
        return row['id']

    def updated_square_string(self, participant_id, string):
        return "UQID{}_{}".format(participant_id, string[2:4])

    def question_from_row(self, row, question_id, question_export_tag):
        Q = copy.deepcopy(self.const_question_template)

        # Add question text.
        pid = self.participant_id(row)

        if self.questionText == False:
            text = "Insert text here."
        else:
            text = self.question_text(row)
        
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
            C["Display"] = self.updated_square_string(pid, C["Display"])

        regions = Q["Payload"]["Regions"]
        for R in regions:
            R["Description"] = self.updated_square_string(pid, R["Description"])
        
        return Q


    def add_question_to_survey(self, survey, question, block_id, participant_id):

        # Add question to survey pile.
        survey["SurveyElements"].append(question)

        # Create a new block to harbor the question.
        new_block = copy.deepcopy(self.const_block_template)
        
        new_block_entry = {
        "Type": "Question",
        "QuestionID": question["Payload"]["QuestionID"]
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
    #endregion

    #region generate survey
    def generate(self):
        gap    = 25
        width  = 50
        height = width
        origin_coordinates = [200, 420]

        generate_image_file = False 

        max_survey_size = None

        self.correct_squares(gap    = gap,
                    width  = width,
                    height = height,
                    origin_coordinates = origin_coordinates,
                    generate_image_file = generate_image_file)
        
        survey = copy.deepcopy(self.const_survey_template)
        num_questions_included = 0
        partition_number = 0

        for i, row in self.setup.iterrows():

            if max_survey_size and num_questions_included >= max_survey_size:

                # Write current batch
                with open("{}.qsf".format(self.output_file), "w") as F:
                    json.dump(survey, F)

                # Reset counters and survey object.
                survey = copy.deepcopy(self.const_survey_template)
                num_questions_included = 0
                partition_number += 1


            pid = self.participant_id(row)

            # vishal: check the code below
            Q = self.question_from_row(row, "QID{}".format(1000 + i), "ID_{}".format(row["id"]))
            self.add_question_to_survey(survey, Q, "BL_MamboNo{}".format(i), pid)

            # Update counter.
            num_questions_included += 1
                
        # Write file
        with open("{}.qsf".format(self.output_file), "w") as F:
            json.dump(survey, F)

        #endregion

    #region extract data
    def subsetQualtrics(self, data, identifier):
        identifierStart = "UQID{}".format(identifier)
        data = data.filter(regex=identifierStart)
        colNames = data.columns
        # split the column names on identifierStart
        data.columns = [col.split(identifierStart)[1] for col in colNames]
        return data


    def extractQualtrics(self, outputFileDir, setup):
        # check if setup is an instance of list or tuple
        if isinstance(setup, str):
            # check if setup is csv or xlsx
            if setup.endswith('.csv') or setup.endswith('.xlsx'):
                setup = pd.read_csv(setup) if setup.endswith('.csv') else pd.read_excel(setup)
                setup = setup['id'].to_list()
        elif isinstance(setup, (list, tuple)):
            if len(setup) != len(set(setup)):
                raise ValueError("All elements in the list or tuple should be unique.")
            setup = setup
        else:
            raise ValueError("Invalid setup format. Please use csv, xlsx, list or tuple.")

        
        # check if outputFile is csv or xlsx
        if outputFileDir.endswith('.csv') or outputFileDir.endswith('.xlsx'):
            output = pd.read_csv(outputFileDir, header=None) if outputFileDir.endswith('.csv') else pd.read_excel(outputFileDir, header=None)
        else:
            raise ValueError("Invalid file format for the qualtrics file. Please use csv or xlsx file format.")
    
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
                qSubset = self.subsetQualtrics(copy.deepcopy(tempRow), identifier)
                temptemp = qSubset[qSubset == "On"]
                temp = temptemp.dropna(axis=1, how='all').columns[0] if not temptemp.dropna(axis=1, how='all').empty else "nan"
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

        output = pd.DataFrame(list(zip(uid, probeid, dc, ac)), columns=['uid', 'Probe Identifier', 'Deliberate Constraints', 'Automatic Constraints'])
        return output


    #endregion