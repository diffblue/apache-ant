package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Recorder.ActionChoices;
import org.apache.tools.ant.taskdefs.Recorder.VerbosityLevelChoices;
import org.junit.Test;

public class RecorderDiffblueTest {
  /**
   * Test ActionChoices getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ActionChoices}
   *   <li>{@link ActionChoices#getValues()}
   * </ul>
   */
  @Test
  public void testActionChoicesGettersAndSetters() {
    // Arrange and Act
    ActionChoices actualActionChoices = new ActionChoices();
    String[] actualValues = actualActionChoices.getValues();

    // Assert
    assertNull(actualActionChoices.getValue());
    assertEquals(-1, actualActionChoices.getIndex());
    assertArrayEquals(new String[]{"start", "stop"}, actualValues);
  }

  /**
   * Test {@link Recorder#init()}.
   * <ul>
   *   <li>Then {@link Recorder} (default constructor) Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Recorder#init()}
   */
  @Test
  public void testInit_thenRecorderProjectBuildListenersSizeIsOne() {
    // Arrange
    Recorder recorder = new Recorder();
    recorder.setProject(new Project());

    // Act
    recorder.init();

    // Assert
    Vector<BuildListener> buildListeners = recorder.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(recorder, buildListeners.get(0));
  }

  /**
   * Test {@link Recorder#init()}.
   * <ul>
   *   <li>Then {@link Recorder} (default constructor) Project BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Recorder#init()}
   */
  @Test
  public void testInit_thenRecorderProjectBuildListenersSizeIsTwo() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    Recorder recorder = new Recorder();
    recorder.setProject(project);

    // Act
    recorder.init();

    // Assert
    Vector<BuildListener> buildListeners = recorder.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
    assertSame(recorder, buildListeners.get(1));
  }

  /**
   * Test {@link Recorder#execute()}.
   * <ul>
   *   <li>Given {@link Recorder} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Recorder#execute()}
   */
  @Test
  public void testExecute_givenRecorder_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Recorder()).execute());
  }

  /**
   * Test new {@link Recorder} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Recorder}
   */
  @Test
  public void testNewRecorder() {
    // Arrange and Act
    Recorder actualRecorder = new Recorder();

    // Assert
    Location location = actualRecorder.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRecorder.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualRecorder.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualRecorder.getTaskName());
    assertNull(actualRecorder.getTaskType());
    assertNull(actualRecorder.getProject());
    assertNull(actualRecorder.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualRecorder, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test VerbosityLevelChoices new {@link VerbosityLevelChoices} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link VerbosityLevelChoices}
   */
  @Test
  public void testVerbosityLevelChoicesNewVerbosityLevelChoices() {
    // Arrange and Act
    VerbosityLevelChoices actualVerbosityLevelChoices = new VerbosityLevelChoices();

    // Assert
    assertNull(actualVerbosityLevelChoices.getValue());
    assertEquals(-1, actualVerbosityLevelChoices.getIndex());
  }
}
