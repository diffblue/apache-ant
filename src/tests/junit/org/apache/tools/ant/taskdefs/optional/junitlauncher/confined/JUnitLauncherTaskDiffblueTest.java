package org.apache.tools.ant.taskdefs.optional.junitlauncher.confined;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.ExecuteWatchdog;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class JUnitLauncherTaskDiffblueTest {
  /**
   * Test new {@link JUnitLauncherTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JUnitLauncherTask}
   */
  @Test
  public void testNewJUnitLauncherTask() {
    // Arrange and Act
    JUnitLauncherTask actualJUnitLauncherTask = new JUnitLauncherTask();

    // Assert
    Location location = actualJUnitLauncherTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJUnitLauncherTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJUnitLauncherTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJUnitLauncherTask.getTaskName());
    assertNull(actualJUnitLauncherTask.getTaskType());
    assertNull(actualJUnitLauncherTask.getProject());
    assertNull(actualJUnitLauncherTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJUnitLauncherTask, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link JUnitLauncherTask#execute()}.
   * <p>
   * Method under test: {@link JUnitLauncherTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();
    jUnitLauncherTask.addConfiguredClassPath(new Path(new Project(), "org/junit/platform/engine/TestEngine.class"));
    jUnitLauncherTask.addConfiguredTest(new SingleTestClass());

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitLauncherTask.execute());
  }

  /**
   * Test {@link JUnitLauncherTask#execute()}.
   * <ul>
   *   <li>Given {@link JUnitLauncherTask} (default constructor) addConfiguredClassPath {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#execute()}
   */
  @Test
  public void testExecute_givenJUnitLauncherTaskAddConfiguredClassPathPathWithProjectIsProject() throws BuildException {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();
    jUnitLauncherTask.addConfiguredClassPath(new Path(new Project()));
    jUnitLauncherTask.addConfiguredTest(new SingleTestClass());

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitLauncherTask.execute());
  }

  /**
   * Test {@link JUnitLauncherTask#execute()}.
   * <ul>
   *   <li>Given {@link JUnitLauncherTask} (default constructor) addConfiguredClassPath {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#execute()}
   */
  @Test
  public void testExecute_givenJUnitLauncherTaskAddConfiguredClassPathSystemBootClasspath() throws BuildException {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();
    jUnitLauncherTask.addConfiguredClassPath(Path.systemBootClasspath);
    jUnitLauncherTask.addConfiguredTest(new SingleTestClass());

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitLauncherTask.execute());
  }

  /**
   * Test {@link JUnitLauncherTask#execute()}.
   * <ul>
   *   <li>Given {@link JUnitLauncherTask} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#execute()}
   */
  @Test
  public void testExecute_givenJUnitLauncherTaskProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();
    jUnitLauncherTask.setProject(new Project());
    jUnitLauncherTask.addConfiguredTest(new SingleTestClass());

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitLauncherTask.execute());
  }

  /**
   * Test {@link JUnitLauncherTask#execute()}.
   * <ul>
   *   <li>Given {@link JUnitLauncherTask} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#execute()}
   */
  @Test
  public void testExecute_givenJUnitLauncherTaskProjectIsProject_thenThrowBuildException2() throws BuildException {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();
    jUnitLauncherTask.setProject(new Project());
    jUnitLauncherTask.addConfiguredClassPath(Path.systemBootClasspath);
    jUnitLauncherTask.addConfiguredTest(new SingleTestClass());

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitLauncherTask.execute());
  }

  /**
   * Test {@link JUnitLauncherTask#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();
    jUnitLauncherTask.addConfiguredTest(new SingleTestClass());

    // Act and Assert
    assertThrows(BuildException.class, () -> jUnitLauncherTask.execute());
  }

  /**
   * Test {@link JUnitLauncherTask#addConfiguredTest(SingleTestClass)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>When {@link SingleTestClass} (default constructor) FailureProperty is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#addConfiguredTest(SingleTestClass)}
   */
  @Test
  public void testAddConfiguredTest_givenNull_whenSingleTestClassFailurePropertyIsNull() {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();

    SingleTestClass test = new SingleTestClass();
    test.setHaltOnFailure(true);
    test.setFailureProperty(null);

    // Act
    jUnitLauncherTask.addConfiguredTest(test);

    // Assert that nothing has changed
    assertTrue(test.getHaltOnFailure());
  }

  /**
   * Test {@link JUnitLauncherTask#addConfiguredTest(SingleTestClass)}.
   * <ul>
   *   <li>Given {@code Test}.</li>
   *   <li>When {@link SingleTestClass} (default constructor) FailureProperty is {@code Test}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#addConfiguredTest(SingleTestClass)}
   */
  @Test
  public void testAddConfiguredTest_givenTest_whenSingleTestClassFailurePropertyIsTest() {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();

    SingleTestClass test = new SingleTestClass();
    test.setHaltOnFailure(true);
    test.setFailureProperty("Test");

    // Act
    jUnitLauncherTask.addConfiguredTest(test);

    // Assert that nothing has changed
    assertTrue(test.getHaltOnFailure());
  }

  /**
   * Test {@link JUnitLauncherTask#addConfiguredTest(SingleTestClass)}.
   * <ul>
   *   <li>When {@link SingleTestClass} (default constructor).</li>
   *   <li>Then not {@link SingleTestClass} (default constructor) HaltOnFailure.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#addConfiguredTest(SingleTestClass)}
   */
  @Test
  public void testAddConfiguredTest_whenSingleTestClass_thenNotSingleTestClassHaltOnFailure() {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();
    SingleTestClass test = new SingleTestClass();

    // Act
    jUnitLauncherTask.addConfiguredTest(test);

    // Assert
    assertFalse(test.getHaltOnFailure());
  }

  /**
   * Test {@link JUnitLauncherTask#addConfiguredTestClasses(TestClasses)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>When {@link TestClasses} (default constructor) FailureProperty is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#addConfiguredTestClasses(TestClasses)}
   */
  @Test
  public void testAddConfiguredTestClasses_givenNull_whenTestClassesFailurePropertyIsNull() {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();

    TestClasses testClasses = new TestClasses();
    testClasses.setHaltOnFailure(true);
    testClasses.setFailureProperty(null);

    // Act
    jUnitLauncherTask.addConfiguredTestClasses(testClasses);

    // Assert that nothing has changed
    assertTrue(testClasses.getHaltOnFailure());
  }

  /**
   * Test {@link JUnitLauncherTask#addConfiguredTestClasses(TestClasses)}.
   * <ul>
   *   <li>Given {@code Test Classes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#addConfiguredTestClasses(TestClasses)}
   */
  @Test
  public void testAddConfiguredTestClasses_givenTestClasses() {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();

    TestClasses testClasses = new TestClasses();
    testClasses.setHaltOnFailure(true);
    testClasses.setFailureProperty("Test Classes");

    // Act
    jUnitLauncherTask.addConfiguredTestClasses(testClasses);

    // Assert that nothing has changed
    assertTrue(testClasses.getHaltOnFailure());
  }

  /**
   * Test {@link JUnitLauncherTask#addConfiguredTestClasses(TestClasses)}.
   * <ul>
   *   <li>When {@link TestClasses} (default constructor).</li>
   *   <li>Then not {@link TestClasses} (default constructor) HaltOnFailure.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#addConfiguredTestClasses(TestClasses)}
   */
  @Test
  public void testAddConfiguredTestClasses_whenTestClasses_thenNotTestClassesHaltOnFailure() {
    // Arrange
    JUnitLauncherTask jUnitLauncherTask = new JUnitLauncherTask();
    TestClasses testClasses = new TestClasses();

    // Act
    jUnitLauncherTask.addConfiguredTestClasses(testClasses);

    // Assert
    assertFalse(testClasses.getHaltOnFailure());
  }

  /**
   * Test {@link JUnitLauncherTask#createExecuteWatchdog(long)}.
   * <ul>
   *   <li>When ten.</li>
   *   <li>Then return not Watching.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherTask#createExecuteWatchdog(long)}
   */
  @Test
  public void testCreateExecuteWatchdog_whenTen_thenReturnNotWatching() {
    // Arrange and Act
    ExecuteWatchdog actualCreateExecuteWatchdogResult = (new JUnitLauncherTask()).createExecuteWatchdog(10L);

    // Assert
    assertFalse(actualCreateExecuteWatchdogResult.isWatching());
    assertFalse(actualCreateExecuteWatchdogResult.killedProcess());
  }
}
