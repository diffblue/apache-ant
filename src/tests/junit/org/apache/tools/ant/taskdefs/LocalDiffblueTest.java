package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.property.LocalProperties;
import org.apache.tools.ant.taskdefs.Local.Name;
import org.junit.Test;

public class LocalDiffblueTest {
  /**
   * Test {@link Local#execute()}.
   * <ul>
   *   <li>Given {@link Local} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Local#execute()}
   */
  @Test
  public void testExecute_givenLocal_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Local()).execute());
  }

  /**
   * Test Name {@link Name#accept(LocalProperties)} with {@code LocalProperties}.
   * <ul>
   *   <li>Given {@link Name} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#accept(LocalProperties)}
   */
  @Test
  public void testNameAcceptWithLocalProperties_givenName_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Name()).accept(null));
  }

  /**
   * Test new {@link Local} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Local}
   */
  @Test
  public void testNewLocal() {
    // Arrange and Act
    Local actualLocal = new Local();

    // Assert
    Location location = actualLocal.getLocation();
    assertNull(location.getFileName());
    assertNull(actualLocal.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualLocal.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualLocal.getTaskName());
    assertNull(actualLocal.getTaskType());
    assertNull(actualLocal.getProject());
    assertNull(actualLocal.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualLocal, runtimeConfigurableWrapper.getProxy());
  }
}
