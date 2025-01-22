package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Manifest.Attribute;
import org.apache.tools.ant.taskdefs.ManifestTask.Mode;
import org.junit.Test;

public class ManifestTaskDiffblueTest {
  /**
   * Test Mode {@link Mode#getValues()}.
   * <p>
   * Method under test: {@link Mode#getValues()}
   */
  @Test
  public void testModeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"update", "replace"}, (new Mode()).getValues());
  }

  /**
   * Test Mode new {@link Mode} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Mode}
   */
  @Test
  public void testModeNewMode() {
    // Arrange and Act
    Mode actualMode = new Mode();

    // Assert
    assertNull(actualMode.getValue());
    assertEquals(-1, actualMode.getIndex());
  }

  /**
   * Test new {@link ManifestTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ManifestTask}
   */
  @Test
  public void testNewManifestTask() {
    // Arrange and Act
    ManifestTask actualManifestTask = new ManifestTask();

    // Assert
    Location location = actualManifestTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualManifestTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualManifestTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualManifestTask.getTaskName());
    assertNull(actualManifestTask.getTaskType());
    assertNull(actualManifestTask.getProject());
    assertNull(actualManifestTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualManifestTask, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link ManifestTask#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestTask#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute_thenThrowBuildException() throws ManifestException {
    // Arrange
    ManifestTask manifestTask = new ManifestTask();

    // Act and Assert
    assertThrows(BuildException.class, () -> manifestTask.addConfiguredAttribute(new Attribute(" ", "42")));
  }

  /**
   * Test {@link ManifestTask#execute()}.
   * <ul>
   *   <li>Given {@link ManifestTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ManifestTask#execute()}
   */
  @Test
  public void testExecute_givenManifestTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ManifestTask()).execute());
  }
}
