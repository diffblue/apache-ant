package org.apache.tools.ant.input;

import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class PropertyFileInputHandlerDiffblueTest {
  /**
   * Test {@link PropertyFileInputHandler#handleInput(InputRequest)}.
   * <ul>
   *   <li>When {@link InputRequest#InputRequest(String)} with {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyFileInputHandler#handleInput(InputRequest)}
   */
  @Test
  public void testHandleInput_whenInputRequestWithPrompt() throws BuildException {
    // Arrange
    PropertyFileInputHandler propertyFileInputHandler = new PropertyFileInputHandler();

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyFileInputHandler.handleInput(new InputRequest("Prompt")));
  }

  /**
   * Test {@link PropertyFileInputHandler#handleInput(InputRequest)}.
   * <ul>
   *   <li>When {@link InputRequest#InputRequest(String)} with prompt is {@link PropertyFileInputHandler#FILE_NAME_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyFileInputHandler#handleInput(InputRequest)}
   */
  @Test
  public void testHandleInput_whenInputRequestWithPromptIsFile_name_key() throws BuildException {
    // Arrange
    PropertyFileInputHandler propertyFileInputHandler = new PropertyFileInputHandler();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> propertyFileInputHandler.handleInput(new InputRequest(PropertyFileInputHandler.FILE_NAME_KEY)));
  }
}
