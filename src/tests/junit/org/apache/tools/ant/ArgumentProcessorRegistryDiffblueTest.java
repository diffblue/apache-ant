package org.apache.tools.ant;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class ArgumentProcessorRegistryDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ArgumentProcessorRegistry#getInstance()}
   *   <li>{@link ArgumentProcessorRegistry#getProcessors()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ArgumentProcessorRegistry actualInstance = ArgumentProcessorRegistry.getInstance();
    ArgumentProcessorRegistry actualInstance2 = actualInstance.getInstance();

    // Assert
    assertTrue(actualInstance.getProcessors().isEmpty());
    assertSame(actualInstance, actualInstance2);
  }

  /**
   * Test {@link ArgumentProcessorRegistry#registerArgumentProcessor(String)} with {@code helperClassName}.
   * <p>
   * Method under test: {@link ArgumentProcessorRegistry#registerArgumentProcessor(String)}
   */
  @Test
  public void testRegisterArgumentProcessorWithHelperClassName() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> ArgumentProcessorRegistry.getInstance().registerArgumentProcessor("Helper Class Name"));
  }

  /**
   * Test {@link ArgumentProcessorRegistry#registerArgumentProcessor(Class)} with {@code helperClass}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArgumentProcessorRegistry#registerArgumentProcessor(Class)}
   */
  @Test
  public void testRegisterArgumentProcessorWithHelperClass_thenThrowBuildException() throws BuildException {
    // Arrange
    ArgumentProcessorRegistry instance = ArgumentProcessorRegistry.getInstance();
    Class<ArgumentProcessor> helperClass = ArgumentProcessor.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> instance.registerArgumentProcessor(helperClass));
  }
}
