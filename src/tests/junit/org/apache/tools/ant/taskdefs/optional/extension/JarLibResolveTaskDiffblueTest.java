package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class JarLibResolveTaskDiffblueTest {
  /**
   * Test {@link JarLibResolveTask#execute()}.
   * <ul>
   *   <li>Given {@link JarLibResolveTask} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JarLibResolveTask#execute()}
   */
  @Test
  public void testExecute_givenJarLibResolveTask() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JarLibResolveTask()).execute());
  }

  /**
   * Test {@link JarLibResolveTask#execute()}.
   * <ul>
   *   <li>Given {@link JarLibResolveTask} (default constructor) Property is {@code Property attribute must be specified.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarLibResolveTask#execute()}
   */
  @Test
  public void testExecute_givenJarLibResolveTaskPropertyIsPropertyAttributeMustBeSpecified() throws BuildException {
    // Arrange
    JarLibResolveTask jarLibResolveTask = new JarLibResolveTask();
    jarLibResolveTask.setProperty("Property attribute must be specified.");

    // Act and Assert
    assertThrows(BuildException.class, () -> jarLibResolveTask.execute());
  }

  /**
   * Test new {@link JarLibResolveTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JarLibResolveTask}
   */
  @Test
  public void testNewJarLibResolveTask() {
    // Arrange and Act
    JarLibResolveTask actualJarLibResolveTask = new JarLibResolveTask();

    // Assert
    Location location = actualJarLibResolveTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJarLibResolveTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJarLibResolveTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJarLibResolveTask.getTaskName());
    assertNull(actualJarLibResolveTask.getTaskType());
    assertNull(actualJarLibResolveTask.getProject());
    assertNull(actualJarLibResolveTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJarLibResolveTask, runtimeConfigurableWrapper.getProxy());
  }
}
