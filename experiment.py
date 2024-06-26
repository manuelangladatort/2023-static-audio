import json
from markupsafe import Markup

import psynet.experiment
from psynet.asset import S3Storage
from psynet.consent import NoConsent, MainConsent
from psynet.modular_page import AudioPrompt, ModularPage, PushButtonControl
from psynet.page import InfoPage, SuccessfulEndPage, VolumeCalibration
from psynet.timeline import Timeline, Event
from psynet.trial import compile_nodes_from_directory
from psynet.trial.static import StaticNode, StaticTrial, StaticTrialMaker
from psynet.utils import get_logger
from psynet.prescreen import AntiphaseHeadphoneTest
from .questionnaire import debrief, questionnaire, STOMPR, TIPI

logger = get_logger()


########################################################################################################################
# Global params
########################################################################################################################
RECRUITER = "prolific"  # "hotair" for sharing with others, "prolific" for deploying

INITIAL_RECRUITMENT_SIZE = 3
TARGET_NUM_PARTICIPANTS = 100

# AUDIO_SET = "audio_data_original.json"  # original audios
AUDIO_SET = "audio_data_synth_f0.json"  # nori's synth_f0 versions

# load json file with audio urls (from s3)
with open(AUDIO_SET) as f:
    validation_data = json.load(f)

nodes = []
for item in validation_data:
    nodes.append(
        StaticNode(
            definition=item,
        )
    )

TRIALS_PER_PARTICIPANT = len(validation_data)
# TRIALS_PER_PARTICIPANT = 5
N_REPEAT_TRIALS = 6
TIME_ESTIMATE_TRIAL = 20


########################################################################################################################
# Create audio stimuli
########################################################################################################################
# upload files from dir to s3
# aws s3 sync local-folder/. s3://bucket/folder/sub-folder
# this will sync everything in your local folder (local-folder) to bucket/folder/sub-folder
# for example
# aws s3 sync /Users/manu/Documents/cap/PROJECTS/MELODY/NHS/NHS_stimuli_set  s3://manu-projects/melody-preferences/NHS


# create a json file with the urls of all audios
# import os
# directory_path = "/Users/manu/Documents/cap/PROJECTS/MELODY/NHS/NHS_stimuli_set/synth_f0"  # to my local folder
# s3_path = "https://manu-projects.s3.amazonaws.com/melody-preferences/NHS/synth_f0/"  # to my s3 project folder
#
# # List all files in the directory
# audios = []
# for filename in os.listdir(directory_path):
#     if filename.lower().endswith(".wav"):
#         full_path = s3_path + filename
#         audio_dict = {
#             "audio_name": filename,
#             "audio_url": full_path
#         }
#         audios.append(audio_dict)
#
# with open("audio_data_synth_f0.json", 'w') as f:
#     json.dump(audios, f)
# f.close()


########################################################################################################################
# Experiment parts
########################################################################################################################
def welcome():
    return InfoPage(
        Markup(
            """
            <h3>Welcome!</h3>
            <hr>
            In this experiment you will be played melodies and asked to rate how much you like them.
            <hr>
            """
        ),
        time_estimate=3
    )


def requirements():
    return InfoPage(
        Markup(
            """
            <h3>Requirements</h3>
            <hr>
            <b><b>For this experiment we need you to use headphones or earplugs and a working microphone.</b></b>
            <br><br>
            If you cannot satisfy these requirements or your microphone or your headphones/earplugs do not work, please return the survey now.
            <hr>
            """
        ),
        time_estimate=3
    )


def instructions_experiment():
    return InfoPage(
        Markup(
            f"""
            <h3>Main experiment</h3>
            <hr>
            You will now start with the main part of the experiment. 
            You will rate a total of {(TRIALS_PER_PARTICIPANT + N_REPEAT_TRIALS)} melodies.
            <br><br>
            <b><b>Please listen to each melody carefully and rate how much you like it</b></b>,
            using a 'pleasantness' scale from 1 (very unpleasant) to 7 (very pleasant).
            <br><br>
            Press <b><b>next</b></b> to start.
            <hr>
            """
        ),
        time_estimate=3
    )


class RatingTrial(StaticTrial):
    time_estimate = TIME_ESTIMATE_TRIAL

    def show_trial(self, experiment, participant):

        # debugger
        # import pydevd_pycharm
        # pydevd_pycharm.settrace('localhost', port=52218, stdoutToServer=True, stderrToServer=True)

        current_trial = self.position + 1

        n_trials = TRIALS_PER_PARTICIPANT + N_REPEAT_TRIALS
        audio_url = self.definition['audio_url']
        show_current_trial = f'<i>Trial number {current_trial} out of {n_trials} trials.</i>'

        logger.info("audio url: {}".format(audio_url))

        return ModularPage(
            "question_page",
            AudioPrompt(
                # audio
                audio_url,
                # text
                Markup(f"""<h5>How much do you like the melody?</h5> <i>{show_current_trial}</i>""")
            ),
            PushButtonControl(
                choices=[1, 2, 3, 4, 5, 6, 7],
                labels=[
                    "(1) Very unpleasant",
                    "(2)",
                    "(3)",
                    "(4)",
                    "(5)",
                    "(6)",
                    "(7) Very pleasant",
                ],
                arrange_vertically=True,
            ),
            events={
                "responseEnable": Event(is_triggered_by="promptEnd"),
                "submitEnable": Event(is_triggered_by="promptEnd")
            }
        )


########################################################################################################################
# Timeline
########################################################################################################################
def get_prolific_settings():
    with open("qualification_prolific.json", "r") as f:
        qualification = json.dumps(json.load(f))
    return {
        "recruiter": "prolific",
        "prolific_estimated_completion_minutes": 28,
        "prolific_maximum_allowed_minutes": 48,
        "prolific_recruitment_config": qualification,
        "base_payment": 3,
        "currency": "£",
        "wage_per_hour": 0.01
    }


class Exp(psynet.experiment.Experiment):
    label = "melody_preferences"
    variables = {
        "soft_max_experiment_payment": 2000.0,
        "hard_max_experiment_payment": 2500.0
    }
    asset_storage = S3Storage(
        "psynet-tests", "static-audio"
    )

    config = {
        **get_prolific_settings(),
        "initial_recruitment_size": INITIAL_RECRUITMENT_SIZE,
        "title": "Listen to music and rate it (20 min, bonus: £3).",
        "description": "Working headphones are required!"
                       "You will hear melodies and be asked to rate them."
                       "The experiment will take approximately 20 min and you will get £3.",
        "contact_email_on_error": "manuel.anglada-tort@ae.mpg.de",
        "organization_name": "Max Planck Institute for Empirical Aesthetics",
        # "dashboard_password": "capcapcap2021!",
        # "dashboard_user": "cap",
        # "dyno_type": "performance-l",
        # "num_dynos_web": 3,
        # "num_dynos_worker": 2,
        # "redis_size": "premium-3",
        # "host": "0.0.0.0",
        # "clock_on": True,
        # "heroku_python_version": "3.10.6",
        # "database_url": "postgresql://postgres@localhost/dallinger",
        # "database_size": "standard-2",
        # "docker_image_base_name": "docker.io/manuelangladatort/static_audio",
        "show_reward": False,
        # "show_progress_bar": False
        "force_incognito_mode": True
    }
    timeline = Timeline(
        MainConsent(),
        welcome(),
        requirements(),
        VolumeCalibration(),
        AntiphaseHeadphoneTest(),
        instructions_experiment(),
        InfoPage(
            Markup(f"""
                <h3>Important note</h3>
                <hr>
                In this experiment, we have removed all the lyrics from the songs. Please focus on rating the musicality of the melodies only.
                <br><br>
                Press <b><b>next</b></b> to start.
                <hr>
                """), time_estimate = 3),
        StaticTrialMaker(
            id_="audio_experiment",
            trial_class=RatingTrial,
            nodes=nodes,
            target_n_participants=TARGET_NUM_PARTICIPANTS,
            recruit_mode="n_participants",
            expected_trials_per_participant=TRIALS_PER_PARTICIPANT,
            max_trials_per_participant=TRIALS_PER_PARTICIPANT,
            n_repeat_trials=N_REPEAT_TRIALS,
        ),
        questionnaire(),
        SuccessfulEndPage(),
    )
