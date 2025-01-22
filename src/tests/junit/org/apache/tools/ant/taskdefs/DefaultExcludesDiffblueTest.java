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

public class DefaultExcludesDiffblueTest {
  /**
   * Test {@link DefaultExcludes#execute()}.
   * <ul>
   *   <li>Given {@link DefaultExcludes} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultExcludes#execute()}
   */
  @Test
  public void testExecute_givenDefaultExcludes_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new DefaultExcludes()).execute());
  }

  /**
   * Test new {@link DefaultExcludes} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DefaultExcludes}
   */
  @Test
  public void testNewDefaultExcludes() {
    // Arrange and Act
    DefaultExcludes actualDefaultExcludes = new DefaultExcludes();

    // Assert
    Location location = actualDefaultExcludes.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDefaultExcludes.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualDefaultExcludes.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualDefaultExcludes.getTaskName());
    assertNull(actualDefaultExcludes.getTaskType());
    assertNull(actualDefaultExcludes.getProject());
    assertNull(actualDefaultExcludes.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualDefaultExcludes, runtimeConfigurableWrapper.getProxy());
  }
}
