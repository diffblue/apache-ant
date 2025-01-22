package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class CVSPassDiffblueTest {
  /**
   * Test new {@link CVSPass} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CVSPass}
   */
  @Test
  public void testNewCVSPass() {
    // Arrange and Act
    CVSPass actualCvsPass = new CVSPass();

    // Assert
    Location location = actualCvsPass.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCvsPass.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualCvsPass.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualCvsPass.getTaskName());
    assertNull(actualCvsPass.getTaskType());
    assertNull(actualCvsPass.getProject());
    assertNull(actualCvsPass.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualCvsPass, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link CVSPass#execute()}.
   * <ul>
   *   <li>Given {@link CVSPass} (default constructor) Password is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CVSPass#execute()}
   */
  @Test
  public void testExecute_givenCVSPassPasswordIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    CVSPass cvsPass = new CVSPass();
    cvsPass.setCvsroot("foo");
    cvsPass.setPassword(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsPass.execute());
  }

  /**
   * Test {@link CVSPass#execute()}.
   * <ul>
   *   <li>Given {@link CVSPass} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CVSPass#execute()}
   */
  @Test
  public void testExecute_givenCVSPass_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CVSPass()).execute());
  }
}
