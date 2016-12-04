"""Contains classes to pre-process text using the jinja library."""

# Library
import jinja2
from symbolic.preprocessors import DefaultPreprocessor

class JinjaPreprocessor(DefaultPreprocessor):
    """
    A jinja-based pre-processor.
    
    Attributes:
        jinjaEnv (jinja2.Environment): The jinja2 environment to use.
    """

    # Setup the default Jinja environment
    jinjaEnv = jinja2.Environment(
        block_start_string='{%',
        block_end_string='%}',
        variable_start_string='{{',
        variable_end_string='}}',
        comment_start_string='/*',
        comment_end_string='*/',
        line_statement_prefix='#',
        line_comment_prefix='//'
    )

    def run(self, text, libName, ppt):
        """
        Pre-process a given text snippet.

        Runs the external program to do the pre-processing.

        Args:
            text (str): The text to pre-process.
            libName (str): The library name.
            ppt (symbolic.preprocessors.PPT): The pre-processor table.
        Returns:
            str: The pre-processed text.
        """
        text = super().run(text, libName)
        template = self.jinjaEnv.from_string(text, ppt.contents, jinja2.Template)
        return template.render(ppt)