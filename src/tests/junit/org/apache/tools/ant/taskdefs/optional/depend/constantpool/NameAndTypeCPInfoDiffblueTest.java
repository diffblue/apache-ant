package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class NameAndTypeCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link NameAndTypeCPInfo}
   *   <li>{@link NameAndTypeCPInfo#getName()}
   *   <li>{@link NameAndTypeCPInfo#getType()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    NameAndTypeCPInfo actualNameAndTypeCPInfo = new NameAndTypeCPInfo();
    String actualName = actualNameAndTypeCPInfo.getName();

    // Assert
    assertNull(actualName);
    assertNull(actualNameAndTypeCPInfo.getType());
    assertEquals(1, actualNameAndTypeCPInfo.getNumEntries());
    assertFalse(actualNameAndTypeCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_NAMEANDTYPE, actualNameAndTypeCPInfo.getTag());
  }

  /**
   * Test {@link NameAndTypeCPInfo#toString()}.
   * <p>
   * Method under test: {@link NameAndTypeCPInfo#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("Name index = 0, descriptor index = 0", (new NameAndTypeCPInfo()).toString());
  }
}
