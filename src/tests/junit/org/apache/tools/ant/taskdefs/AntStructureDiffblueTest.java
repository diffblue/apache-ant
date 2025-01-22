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

public class AntStructureDiffblueTest {
  /**
   * Test {@link AntStructure#execute()}.
   * <ul>
   *   <li>Given {@link AntStructure} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntStructure#execute()}
   */
  @Test
  public void testExecute_givenAntStructure_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new AntStructure()).execute());
  }

  /**
   * Test {@link AntStructure#isNmtoken(String)}.
   * <p>
   * Method under test: {@link AntStructure#isNmtoken(String)}
   */
  @Test
  public void testIsNmtoken() {
    // Arrange, Act and Assert
    assertTrue((new AntStructure()).isNmtoken("foo"));
  }

  /**
   * Test {@link AntStructure#areNmtokens(String[])}.
   * <p>
   * Method under test: {@link AntStructure#areNmtokens(String[])}
   */
  @Test
  public void testAreNmtokens() {
    // Arrange, Act and Assert
    assertTrue((new AntStructure()).areNmtokens(new String[]{"foo"}));
  }

  /**
   * Test new {@link AntStructure} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AntStructure}
   */
  @Test
  public void testNewAntStructure() {
    // Arrange and Act
    AntStructure actualAntStructure = new AntStructure();

    // Assert
    Location location = actualAntStructure.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAntStructure.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualAntStructure.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualAntStructure.getTaskName());
    assertNull(actualAntStructure.getTaskType());
    assertNull(actualAntStructure.getProject());
    assertNull(actualAntStructure.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualAntStructure, runtimeConfigurableWrapper.getProxy());
  }
}
