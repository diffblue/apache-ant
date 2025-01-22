package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.ExitStatusException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.condition.ConditionBase;
import org.junit.Test;

public class ExitDiffblueTest {
  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) If is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitIfIsEmptyString_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setIf((Object) "");

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) If is {@code on}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitIfIsOn_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setIf((Object) "on");

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) If is {@link Boolean#TRUE} toString.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitIfIsTrueToString_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setIf((Object) Boolean.TRUE.toString());

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) If is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitIfIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setIf(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) If is {@code yes}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitIfIsYes_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setIf((Object) "yes");

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) Message is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitMessageIs42_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setMessage("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) Message is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitMessageIsEmptyString_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setMessage("");

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) Status is one.</li>
   *   <li>Then throw {@link ExitStatusException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitStatusIsOne_thenThrowExitStatusException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setStatus(1);

    // Act and Assert
    assertThrows(ExitStatusException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor) Unless is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExitUnlessIs42_thenThrowBuildException() throws BuildException {
    // Arrange
    Exit exit = new Exit();
    exit.setUnless((Object) "42");

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Exit} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenExit_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Exit()).execute());
  }

  /**
   * Test {@link Exit#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exit#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Exit exit = new Exit();
    exit.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> exit.execute());
  }

  /**
   * Test {@link Exit#createCondition()}.
   * <p>
   * Method under test: {@link Exit#createCondition()}
   */
  @Test
  public void testCreateCondition() {
    // Arrange and Act
    ConditionBase actualCreateConditionResult = (new Exit()).createCondition();

    // Assert
    assertEquals("component", actualCreateConditionResult.getTaskName());
    Location location = actualCreateConditionResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateConditionResult.getDescription());
    assertNull(actualCreateConditionResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test new {@link Exit} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Exit}
   */
  @Test
  public void testNewExit() {
    // Arrange and Act
    Exit actualExit = new Exit();

    // Assert
    Location location = actualExit.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExit.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualExit.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualExit.getTaskName());
    assertNull(actualExit.getTaskType());
    assertNull(actualExit.getProject());
    assertNull(actualExit.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualExit, runtimeConfigurableWrapper.getProxy());
  }
}
