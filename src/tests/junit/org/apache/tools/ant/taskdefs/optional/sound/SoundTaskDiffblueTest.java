package org.apache.tools.ant.taskdefs.optional.sound;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.apache.tools.ant.taskdefs.optional.sound.SoundTask.BuildAlert;
import org.junit.Test;

public class SoundTaskDiffblueTest {
  /**
   * Test BuildAlert {@link BuildAlert#getSource()}.
   * <p>
   * Method under test: {@link BuildAlert#getSource()}
   */
  @Test
  public void testBuildAlertGetSource() {
    // Arrange
    BuildAlert buildAlert = (new SoundTask()).new BuildAlert();
    buildAlert.setSource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> buildAlert.getSource());
  }

  /**
   * Test BuildAlert {@link BuildAlert#getSource()}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BuildAlert#getSource()}
   */
  @Test
  public void testBuildAlertGetSource_thenReturnNull() {
    // Arrange
    BuildAlert buildAlert = (new SoundTask()).new BuildAlert();
    buildAlert.setSource(Paths.get(System.getProperty("java.io.tmpdir"), "No files found in directory ").toFile());

    // Act and Assert
    assertNull(buildAlert.getSource());
  }

  /**
   * Test BuildAlert getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link BuildAlert#BuildAlert(SoundTask)}
   *   <li>{@link BuildAlert#setDuration(Long)}
   *   <li>{@link BuildAlert#setLoops(int)}
   *   <li>{@link BuildAlert#setSource(File)}
   *   <li>{@link BuildAlert#getDuration()}
   *   <li>{@link BuildAlert#getLoops()}
   * </ul>
   */
  @Test
  public void testBuildAlertGettersAndSetters() {
    // Arrange and Act
    BuildAlert actualBuildAlert = (new SoundTask()).new BuildAlert();
    actualBuildAlert.setDuration(1L);
    actualBuildAlert.setLoops(1);
    actualBuildAlert.setSource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    Long actualDuration = actualBuildAlert.getDuration();

    // Assert
    assertEquals(1, actualBuildAlert.getLoops());
    assertEquals(1L, actualDuration.longValue());
  }

  /**
   * Test {@link SoundTask#createSuccess()}.
   * <p>
   * Method under test: {@link SoundTask#createSuccess()}
   */
  @Test
  public void testCreateSuccess() {
    // Arrange and Act
    BuildAlert actualCreateSuccessResult = (new SoundTask()).createSuccess();

    // Assert
    assertNull(actualCreateSuccessResult.getDuration());
    assertEquals(0, actualCreateSuccessResult.getLoops());
  }

  /**
   * Test {@link SoundTask#createFail()}.
   * <p>
   * Method under test: {@link SoundTask#createFail()}
   */
  @Test
  public void testCreateFail() {
    // Arrange and Act
    BuildAlert actualCreateFailResult = (new SoundTask()).createFail();

    // Assert
    assertNull(actualCreateFailResult.getDuration());
    assertEquals(0, actualCreateFailResult.getLoops());
  }

  /**
   * Test new {@link SoundTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SoundTask}
   */
  @Test
  public void testNewSoundTask() {
    // Arrange and Act
    SoundTask actualSoundTask = new SoundTask();

    // Assert
    Location location = actualSoundTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSoundTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSoundTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSoundTask.getTaskName());
    assertNull(actualSoundTask.getTaskType());
    assertNull(actualSoundTask.getProject());
    assertNull(actualSoundTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSoundTask, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link SoundTask#execute()}.
   * <ul>
   *   <li>Then {@link SoundTask} (default constructor) Project BuildListeners first {@link BigProjectLogger}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SoundTask#execute()}
   */
  @Test
  public void testExecute_thenSoundTaskProjectBuildListenersFirstBigProjectLogger() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(1)));

    Project project = new Project();
    project.addBuildListener(listener);

    SoundTask soundTask = new SoundTask();
    soundTask.setProject(project);

    // Act
    soundTask.execute();

    // Assert
    Vector<BuildListener> buildListeners = soundTask.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    BuildListener getResult = buildListeners.get(0);
    assertTrue(getResult instanceof BigProjectLogger);
    assertTrue(buildListeners.get(1) instanceof AntSoundPlayer);
    assertSame(listener, getResult);
  }

  /**
   * Test {@link SoundTask#execute()}.
   * <ul>
   *   <li>Then {@link SoundTask} (default constructor) Project BuildListeners first is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SoundTask#execute()}
   */
  @Test
  public void testExecute_thenSoundTaskProjectBuildListenersFirstIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    SoundTask soundTask = new SoundTask();
    soundTask.setProject(project);

    // Act
    soundTask.execute();

    // Assert
    Vector<BuildListener> buildListeners = soundTask.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertTrue(buildListeners.get(1) instanceof AntSoundPlayer);
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link SoundTask#execute()}.
   * <ul>
   *   <li>Then {@link SoundTask} (default constructor) Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SoundTask#execute()}
   */
  @Test
  public void testExecute_thenSoundTaskProjectBuildListenersSizeIsOne() {
    // Arrange
    SoundTask soundTask = new SoundTask();
    soundTask.setProject(new Project());

    // Act
    soundTask.execute();

    // Assert
    Vector<BuildListener> buildListeners = soundTask.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertTrue(buildListeners.get(0) instanceof AntSoundPlayer);
  }
}
