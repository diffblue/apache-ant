package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class ModuleCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ModuleCPInfo}
   *   <li>{@link ModuleCPInfo#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ModuleCPInfo actualModuleCPInfo = new ModuleCPInfo();

    // Assert
    assertEquals("Module info Constant Pool Entry for null[0]", actualModuleCPInfo.toString());
    assertNull(actualModuleCPInfo.getValue());
    assertEquals(1, actualModuleCPInfo.getNumEntries());
    assertFalse(actualModuleCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_MODULEINFO, actualModuleCPInfo.getTag());
  }
}
