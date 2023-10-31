import random
from dominate import tags

from psynet.demography.general import Age, Gender
from psynet.demography.gmsi import GMSI
from psynet.modular_page import ModularPage, TextControl, SurveyJSControl
from psynet.page import InfoPage
from psynet.timeline import join, FailedValidation


def introduction():
    html = tags.div()
    with html:
        tags.p(
            "Congratulations, you completed the listening part of this experiment!"
        )
        tags.p(
            "Before we finish, we would like to ask you a few questions about you. ",
            "They should only take a couple of minutes to complete.",
        )
    return InfoPage(html, time_estimate=10)


def questionnaire():
    return join(
        introduction(),
        Age(),
        Gender(),
        GMSI(subscales=["Musical Training"]),
        InfoPage("Next, we would like to ask you some questions about your music preferences.",
                 time_estimate=3),
        STOMPR(),
        InfoPage("Finally, we would like to ask you some questions about your personality.",
                 time_estimate=3),
        TIPI(),
        DFS(),
        feedback(),
    )


STOMPR_rating_values = [
    {"value": "1", "text": "1 (Dislike Strongly)"},
    {"value": "2", "text": "2"},
    {"value": "3", "text": "3"},
    {"value": "4", "text": "4"},
    {"value": "5", "text": "5"},
    {"value": "6", "text": "6"},
    {"value": "7", "text": "7 (Like Strongly)"}
]

music_genres = [
    {"value": "Alternative", "text": "Alternative"},
    {"value": "Bluegrass", "text": "Bluegrass"},
    {"value": "Blues", "text": "Blues"},
    {"value": "Classical", "text": "Classical"},
    {"value": "Country", "text": "Country"},
    {"value": "Dance/Electronica", "text": "Dance/Electronica"},
    {"value": "Folk", "text": "Folk"},
    {"value": "Funk", "text": "Funk"},
    {"value": "Gospel", "text": "Gospel"},
    {"value": "Heavy Metal", "text": "Heavy Metal"},
    {"value": "World", "text": "World"},
    {"value": "Jazz", "text": "Jazz"},
    {"value": "New Age", "text": "New Age"},
    {"value": "Oldies", "text": "Oldies"},
    {"value": "Opera", "text": "Opera"},
    {"value": "Pop", "text": "Pop"},
    {"value": "Punk", "text": "Punk"},
    {"value": "Rap/hip-hop", "text": "Rap/hip-hop"},
    {"value": "Reggae", "text": "Reggae"},
    {"value": "Religious", "text": "Religious"},
    {"value": "Rock", "text": "Rock"},
    {"value": "Soul/R&B", "text": "Soul/R&B"},
    {"value": "Soundtracks/theme song", "text": "Soundtracks/theme song"}
]


class STOMPR(ModularPage):
    def __init__(self):
        super().__init__(
            "stompr",
            "Rate your preferences for each music genre on a scale from 1 (Dislike Strongly) to 7 (Like Strongly).",
            SurveyJSControl(
                {
                    "logoPosition": "right",
                    "pages": [
                        {
                            "name": "page1",
                            "elements": [
                                {
                                    "type": "matrix",
                                    "name": "STOMPR_choices",
                                    "title": "Please indicate your basic preference for each of the following genres using the scale provided.",
                                    "isRequired": True,
                                    "columns": STOMPR_rating_values,
                                    "rows": music_genres,
                                },
                            ],
                        },
                    ],
                },
            ),
            time_estimate=55,
            bot_response=lambda: {"rating": "5",},
        )

    def validate(self, response, **kwargs):
        n_responses = len(response.answer["STOMPR_choices"])

        if n_responses < len(music_genres):
            return FailedValidation("Please answer all the questions.")

        return None


TIPI_rating_values = [
    {"value": "1", "text": "1 (Disagree Strongly)"},
    {"value": "2", "text": "2"},
    {"value": "3", "text": "3"},
    {"value": "4", "text": "4"},
    {"value": "5", "text": "5"},
    {"value": "6", "text": "6"},
    {"value": "7", "text": "7 (Agree Strongly)"}
]

tipi_genres = [
    {"value": "Extraverted", "text": "Extraverted, enthusiastic"},
    {"value": "Critical", "text": "Critical, quarrelsome."},
    {"value": "Dependable", "text": "Dependable, self-disciplined."},
    {"value": "Anxious", "text": "Anxious, easily upset."},
    {"value": "Open", "text": "Open to new experiences, complex."},
    {"value": "Reserved", "text": "Reserved, quiet."},
    {"value": "Sympathetic", "text": "Sympathetic, warm."},
    {"value": "Disorganized", "text": "Disorganized, careless."},
    {"value": "Calm", "text": "Calm, emotionally stable."},
    {"value": "Conventional", "text": "Conventional, uncreative."},
]

class TIPI(ModularPage):
    def __init__(self):
        super().__init__(
            "tipi",
            "Here are a number of personality traits that may or may not apply to you. Please rate each statement to indicate the extent to which you agree/ disagree with them on a scale from 1 (Disagree Strongly) to 7 (Agree Strongly).",
            SurveyJSControl(
                {
                    "logoPosition": "right",
                    "pages": [
                        {
                            "name": "page1",
                            "elements": [
                                {
                                    "type": "matrix",
                                    "name": "TIPI_choices",
                                    "title": "I see myself as:",
                                    "isRequired": True,
                                    "columns": TIPI_rating_values,
                                    "rows": tipi_genres,
                                },
                            ],
                        },
                    ],
                },
            ),
            time_estimate=55,
            bot_response=lambda: {"rating": "5",},
        )

    def validate(self, response, **kwargs):
        n_responses = len(response.answer["TIPI_choices"])

        if n_responses < len(tipi_genres):
            return FailedValidation("Please answer all the questions.")

        return None


def feedback():
    return ModularPage(
        "feedback",
        "Do you have any feedback to give us about the experiment?",
        TextControl(one_line=False),
        bot_response="I am just a bot, I don't have any feedback for you.",
        save_answer="feedback",
        time_estimate=5,
    )


def debrief():
    html = tags.div()

    with html:
        tags.p(
            """
            Thank you for participating in this experiment. The purpose of the experiment was to collect data on how we 
            perceive ‘pleasant’ melodies (sequences of musical tones), such as the ones you have been listening to.
            """
        )
        tags.p(
            """
            Pleasantness is very important to understand how we perceive and create musical melodies (the main musical 
            idea in a piece of music, or that part that you can sing or hum along to). Other important aspects melodies 
            are pitch (the series of notes that rise and fall in pitch) and rhythm (the timing and duration of these 
            notes).
            """
        )
        tags.p(
            """
            The data collected during this experiment will help to better understand how people derive pleasure from 
            melodies, studying for the first time all possible melodic combinations and listeners' individual 
            differences at a large scale (testing many melodies and participants from different backgrounds).
            """
        )

    return InfoPage(html, time_estimate=10)

DFS_rating_values = [
    {"value": "1", "text": "1 (Never)"},
    {"value": "2", "text": "2"},
    {"value": "3", "text": "3"},
    {"value": "4", "text": "4"},
    {"value": "5", "text": "5 (Always)"}
]

DFS_items = [
    {"value": "Challenge_skill1", "text": "I am challenged, but I believe my skills will allow me to meet the challenge"},
    {"value": "Action_awareness1", "text": "I do things correctly without thinking about trying to do so"},
    {"value": "Clear_goals1", "text": "I know clearly what I want to do"},
    {"value": "Unambiguous_feedback1", "text": "It is really clear to me how I am going"},
    {"value": "Task_concentration1", "text": "My attention is focused entirely on what I am doing"},
    {"value": "Sense_of_control1", "text": "I have a sense of control over what I am doing"},
    {"value": "Loss_of_self1", "text": "I am not concerned with what others may be thinking of me"},
    {"value": "Transformation_of_time1", "text": "Time seems to alter (either slows down or speeds up)"},
    {"value": "Autotelic_experience1", "text": "I really enjoy the experience of what I am doing"},
    {"value": "Challenge_skill2", "text": "My abilities match the challenge of what I am doing"},
    {"value": "Action_awareness2", "text": "Things just seem to happen automatically"},
    {"value": "Clear_goals2", "text": "I have a strong sense of what I want to do"},
    {"value": "Unambiguous_feedback2", "text": "I am aware of how well I am doing"},
    {"value": "Task_concentration2", "text": "It is no effort to keep my mind on what is happening"},
    {"value": "Sense_of_control2", "text": "I feel like I can control what I am doing"},
    {"value": "Transformation_of_time2", "text": "The way time passes seems to be different from normal"},
    {"value": "Autotelic_experience2", "text": "I love the feeling of what I am doing and want to capture this feeling again"},
    {"value": "Challenge_skill3", "text": "I feel I am competent enough to meet the demands of the situation"},
    {"value": "Action_awareness3", "text": "I do things automatically, without thinking too much"},
    {"value": "Clear_goals3", "text": "I know what I want to achieve"},
    {"value": "Unambiguous_feedback3", "text": "I have a good idea about how well I am doing while I am involved in the task/activity"},
    {"value": "Task_concentration3", "text": "I have total connection"},
    {"value": "Sense_of_control3", "text": "I have feeling of total control over what I am doing"},
    {"value": "Transformation_of_time3", "text": "It feels like time goes by quickly"},
    {"value": "Autotelic_experience3", "text": "The experience leaves me feeling great"},
    {"value": "Challenge_skill4", "text": "The challenge and my skills are at an equally high level"},
    {"value": "Action_awareness4", "text": "I do things spontaneously and automatically without having to think"},
    {"value": "Clear_goals4", "text": "My goals are clearly defined"},
    {"value": "Unambiguous_feedback4", "text": "I can tell by the way things are progressing how well I am doing"},
    {"value": "Task_concentration4", "text": "I an completely focused on the task at hand"},
    {"value": "Sense_of_control4", "text": "I feel in total control of my actions"},
    {"value": "Transformation_of_time4", "text": "I lose my normal awareness of time"},
    {"value": "Autotelic_experience4", "text": "The experience is extremely rewarding"},

]


class DFS(ModularPage):
    def __init__(self):
        super().__init__(
            "DFS",
            "Rate your agreement with each statement on a scale from 1 (never) to 5 (always).",
            SurveyJSControl(
                {
                    "logoPosition": "right",
                    "pages": [
                        {
                            "name": "page1",
                            "elements": [
                                {
                                    "type": "matrix",
                                    "name": "DFS_choices",
                                    "title": "Think about how you felt during the task today.",
                                    "isRequired": True,
                                    "columns": DFS_rating_values,
                                    "rows": DFS_items,
                                },
                            ],
                        },
                    ],
                },
            ),
            time_estimate=55,
            bot_response=lambda: {"rating": "5",},
        )
