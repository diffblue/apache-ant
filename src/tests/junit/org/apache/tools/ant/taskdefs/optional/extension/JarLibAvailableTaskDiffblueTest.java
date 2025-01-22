package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class JarLibAvailableTaskDiffblueTest {
  /**
   * Test {@link JarLibAvailableTask#addConfiguredExtension(ExtensionAdapter)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarLibAvailableTask#addConfiguredExtension(ExtensionAdapter)}
   */
  @Test
  public void testAddConfiguredExtension_thenThrowBuildException() {
    // Arrange
    JarLibAvailableTask jarLibAvailableTask = new JarLibAvailableTask();
    jarLibAvailableTask.addConfiguredExtension(new ExtensionAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> jarLibAvailableTask.addConfiguredExtension(new ExtensionAdapter()));
  }

  /**
   * Test {@link JarLibAvailableTask#execute()}.
   * <p>
   * Method under test: {@link JarLibAvailableTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    JarLibAvailableTask jarLibAvailableTask = new JarLibAvailableTask();
    jarLibAvailableTask.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jarLibAvailableTask.addConfiguredExtension(new ExtensionAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> jarLibAvailableTask.execute());
  }

  /**
   * Test {@link JarLibAvailableTask#execute()}.
   * <p>
   * Method under test: {@link JarLibAvailableTask#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    JarLibAvailableTask jarLibAvailableTask = new JarLibAvailableTask();
    jarLibAvailableTask.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "'%s' is not a file.").toFile());
    jarLibAvailableTask.addConfiguredExtension(new ExtensionAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> jarLibAvailableTask.execute());
  }

  /**
   * Test {@link JarLibAvailableTask#execute()}.
   * <ul>
   *   <li>Given {@link JarLibAvailableTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarLibAvailableTask#execute()}
   */
  @Test
  public void testExecute_givenJarLibAvailableTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JarLibAvailableTask()).execute());
  }

  /**
   * Test {@link JarLibAvailableTask#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarLibAvailableTask#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    JarLibAvailableTask jarLibAvailableTask = new JarLibAvailableTask();
    jarLibAvailableTask.addConfiguredExtension(new ExtensionAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> jarLibAvailableTask.execute());
  }

  /**
   * Test new {@link JarLibAvailableTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JarLibAvailableTask}
   */
  @Test
  public void testNewJarLibAvailableTask() {
    // Arrange and Act
    JarLibAvailableTask actualJarLibAvailableTask = new JarLibAvailableTask();

    // Assert
    Location location = actualJarLibAvailableTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJarLibAvailableTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJarLibAvailableTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJarLibAvailableTask.getTaskName());
    assertNull(actualJarLibAvailableTask.getTaskType());
    assertNull(actualJarLibAvailableTask.getProject());
    assertNull(actualJarLibAvailableTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJarLibAvailableTask, runtimeConfigurableWrapper.getProxy());
  }
}
