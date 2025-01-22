package org.apache.tools.ant.taskdefs.optional.testing;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class BlockForDiffblueTest {
  /**
   * Test {@link BlockFor#BlockFor()}.
   * <p>
   * Method under test: {@link BlockFor#BlockFor()}
   */
  @Test
  public void testNewBlockFor() {
    // Arrange and Act
    BlockFor actualBlockFor = new BlockFor();

    // Assert
    assertEquals("blockfor", actualBlockFor.getTaskName());
    Location location = actualBlockFor.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBlockFor.getDescription());
    assertNull(actualBlockFor.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link BlockFor#BlockFor(String)}.
   * <p>
   * Method under test: {@link BlockFor#BlockFor(String)}
   */
  @Test
  public void testNewBlockFor2() {
    // Arrange and Act
    BlockFor actualBlockFor = new BlockFor("Task Name");

    // Assert
    assertEquals("Task Name", actualBlockFor.getTaskName());
    Location location = actualBlockFor.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBlockFor.getDescription());
    assertNull(actualBlockFor.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link BlockFor#processTimeout()}.
   * <ul>
   *   <li>Given {@link BlockFor#BlockFor()} TimeoutProperty is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockFor#processTimeout()}
   */
  @Test
  public void testProcessTimeout_givenBlockForTimeoutPropertyIsFoo() throws BuildTimeoutException {
    // Arrange
    BlockFor blockFor = new BlockFor();
    blockFor.setProject(new Project());
    blockFor.setTimeoutProperty("foo");

    // Act and Assert
    assertThrows(BuildTimeoutException.class, () -> blockFor.processTimeout());
  }

  /**
   * Test {@link BlockFor#processTimeout()}.
   * <ul>
   *   <li>Given {@link BlockFor#BlockFor()} TimeoutProperty is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockFor#processTimeout()}
   */
  @Test
  public void testProcessTimeout_givenBlockForTimeoutPropertyIsNull() throws BuildTimeoutException {
    // Arrange
    BlockFor blockFor = new BlockFor();
    blockFor.setProject(new Project());
    blockFor.setTimeoutProperty(null);

    // Act and Assert
    assertThrows(BuildTimeoutException.class, () -> blockFor.processTimeout());
  }

  /**
   * Test {@link BlockFor#processTimeout()}.
   * <ul>
   *   <li>Given {@link BlockFor#BlockFor()}.</li>
   *   <li>Then throw {@link BuildTimeoutException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockFor#processTimeout()}
   */
  @Test
  public void testProcessTimeout_givenBlockFor_thenThrowBuildTimeoutException() throws BuildTimeoutException {
    // Arrange, Act and Assert
    assertThrows(BuildTimeoutException.class, () -> (new BlockFor()).processTimeout());
  }

  /**
   * Test {@link BlockFor#processTimeout()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildTimeoutException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockFor#processTimeout()}
   */
  @Test
  public void testProcessTimeout_givenJavaLangObject_thenThrowBuildTimeoutException() throws BuildTimeoutException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(": timeout", typeClass);

    BlockFor blockFor = new BlockFor();
    blockFor.setProject(project);
    blockFor.setTimeoutProperty("foo");

    // Act and Assert
    assertThrows(BuildTimeoutException.class, () -> blockFor.processTimeout());
  }

  /**
   * Test {@link BlockFor#processTimeout()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockFor#processTimeout()}
   */
  @Test
  public void testProcessTimeout_givenProjectAddBuildListenerAntClassLoader() throws BuildTimeoutException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    BlockFor blockFor = new BlockFor();
    blockFor.setProject(project);
    blockFor.setTimeoutProperty(null);

    // Act and Assert
    assertThrows(BuildTimeoutException.class, () -> blockFor.processTimeout());
  }
}
