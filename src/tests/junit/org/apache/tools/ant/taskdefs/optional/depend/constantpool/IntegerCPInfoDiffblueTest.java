package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import org.junit.Test;

public class IntegerCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link IntegerCPInfo}
   *   <li>{@link IntegerCPInfo#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    IntegerCPInfo actualIntegerCPInfo = new IntegerCPInfo();

    // Assert
    assertEquals("Integer Constant Pool Entry: null", actualIntegerCPInfo.toString());
    assertNull(actualIntegerCPInfo.getValue());
    assertEquals(1, actualIntegerCPInfo.getNumEntries());
    assertEquals(3, actualIntegerCPInfo.getTag());
    assertFalse(actualIntegerCPInfo.isResolved());
  }

  /**
   * Test {@link IntegerCPInfo#read(DataInputStream)}.
   * <ul>
   *   <li>Then {@link IntegerCPInfo} (default constructor) Value intValue is {@code 1096302936}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntegerCPInfo#read(DataInputStream)}
   */
  @Test
  public void testRead_thenIntegerCPInfoValueIntValueIs1096302936() throws IOException {
    // Arrange
    IntegerCPInfo integerCPInfo = new IntegerCPInfo();

    // Act
    integerCPInfo.read(new DataInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));

    // Assert
    assertEquals(1096302936, ((Integer) integerCPInfo.getValue()).intValue());
  }
}
