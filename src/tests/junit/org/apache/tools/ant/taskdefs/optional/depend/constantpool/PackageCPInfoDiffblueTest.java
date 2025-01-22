package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class PackageCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link PackageCPInfo}
   *   <li>{@link PackageCPInfo#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    PackageCPInfo actualPackageCPInfo = new PackageCPInfo();

    // Assert
    assertEquals("Package info Constant Pool Entry for null[0]", actualPackageCPInfo.toString());
    assertNull(actualPackageCPInfo.getValue());
    assertEquals(1, actualPackageCPInfo.getNumEntries());
    assertFalse(actualPackageCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_PACKAGEINFO, actualPackageCPInfo.getTag());
  }
}
