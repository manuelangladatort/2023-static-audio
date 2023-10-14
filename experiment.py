import random
from markupsafe import Markup

import psynet.experiment
from psynet.asset import DebugStorage
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
TARGET_NUM_PARTICIPANTS = 10

TRIALS_PER_PARTICIPANT = 5
TRIALS_PER_PARTICIPANT_PRACTICE = 2
N_REPEAT_TRIALS = 0

########################################################################################################################
# Experiment parts
########################################################################################################################
def instructions():
    return InfoPage(
        Markup(
            """
            <h3>Instructions</h3>
            <hr>
            In this experiment, you will hear melodies and be asked to rate how much you like them.
            <br><br>
            Press <b><b>next</b></b> when you are ready to start.
            <hr>
            """
        ),
        time_estimate=0
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
        time_estimate=0
    )


class RatingTrial(StaticTrial):
    time_estimate = 10

    def show_trial(self, experiment, participant):

        # Debugger
        # import pydevd_pycharm
        # pydevd_pycharm.settrace('localhost', port=52218, stdoutToServer=True, stderrToServer=True)

        # count number trials per trial
        if self.trial_maker_id == "audio_practice":
            n_trials = TRIALS_PER_PARTICIPANT_PRACTICE
        else:
            n_trials = TRIALS_PER_PARTICIPANT + N_REPEAT_TRIALS

        current_trial = self.position + 1
        show_current_trial = f'<i>Trial number {current_trial} out of {n_trials} trials.</i>'

        return ModularPage(
            "question_page",
            AudioPrompt(self.assets["prompt"],
                        Markup(f"""
                        <h5>How much do you like the song?</h5>
                        <i>{show_current_trial}</i>
                        """
                               )
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
class Exp(psynet.experiment.Experiment):
    label = "Audio stimulus set from directory demo"
    asset_storage = DebugStorage()

    timeline = Timeline(
        NoConsent(),
        # MainConsent(),  # use for main experiment
        InfoPage("Welcome! In this experiment you will be played a series of melodies. Your task will be to rate the 'pleasantness' of these melodies on a numeric scale.", time_estimate=5),
        requirements(),
        # VolumeCalibration(),
        # AntiphaseHeadphoneTest(),
        instructions(),
        StaticTrialMaker(
            id_="audio_practice",
            trial_class=RatingTrial,
            nodes=compile_nodes_from_directory(
                input_dir="input/practice", media_ext=".mp3", node_class=StaticNode
            ),
            target_n_participants=0,
            recruit_mode="n_participants",
            expected_trials_per_participant=TRIALS_PER_PARTICIPANT_PRACTICE,
            max_trials_per_participant=TRIALS_PER_PARTICIPANT_PRACTICE,
        ),
        InfoPage("We continue with the experiment trials.", time_estimate=5),
        StaticTrialMaker(
            id_="audio_experiment",
            trial_class=RatingTrial,
            nodes=compile_nodes_from_directory(
                input_dir="input/experiment", media_ext=".mp3", node_class=StaticNode
            ),
            target_n_participants=TARGET_NUM_PARTICIPANTS,
            recruit_mode="n_participants",
            expected_trials_per_participant=TRIALS_PER_PARTICIPANT,
            max_trials_per_participant=TRIALS_PER_PARTICIPANT,
        ),
        questionnaire(),
        InfoPage("Next, we would like to ask you some questions about your music preferences (0.15 extra bonus)",
                 time_estimate=3),
        STOMPR(),
        InfoPage("Finally, we would like to ask you some questions about your personality (0.15 extra bonus)",
                 time_estimate=3),
        TIPI(),
        SuccessfulEndPage(),
    )
