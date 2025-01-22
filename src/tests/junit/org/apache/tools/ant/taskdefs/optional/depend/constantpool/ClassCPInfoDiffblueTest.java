package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import org.junit.Test;

public class ClassCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ClassCPInfo}
   *   <li>{@link ClassCPInfo#toString()}
   *   <li>{@link ClassCPInfo#getClassName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ClassCPInfo actualClassCPInfo = new ClassCPInfo();
    String actualToStringResult = actualClassCPInfo.toString();

    // Assert
    assertEquals("Class Constant Pool Entry for null[0]", actualToStringResult);
    assertNull(actualClassCPInfo.getClassName());
    assertEquals(1, actualClassCPInfo.getNumEntries());
    assertEquals(7, actualClassCPInfo.getTag());
    assertFalse(actualClassCPInfo.isResolved());
  }

  /**
   * Test {@link ClassCPInfo#read(DataInputStream)}.
   * <ul>
   *   <li>Then {@link ClassCPInfo} (default constructor) ClassName is {@code unresolved}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassCPInfo#read(DataInputStream)}
   */
  @Test
  public void testRead_thenClassCPInfoClassNameIsUnresolved() throws IOException {
    // Arrange
    ClassCPInfo classCPInfo = new ClassCPInfo();

    // Act
    classCPInfo.read(new DataInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));

    // Assert
    assertEquals("unresolved", classCPInfo.getClassName());
  }
}
